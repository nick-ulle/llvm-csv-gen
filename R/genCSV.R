
#' @useDynLib llvm.csv.gen
#' @import Rllvm
NULL


#' readCSV Demo
#'
#' Run a demonstration of the compiled readCSV routine.
#'
#' @export
demo_readCSV = function()
{
  llvmAddSymbol("nextToken")
  m = generate_readCSV(colClasses = list("integer", "numeric", "integer"))
  file = system.file("sample.csv", package = "llvm.csv.gen")
  .llvm(m$readCSV, file, 3L)
}


#' Include stdlib.h
#'
#' Declare structures and routines from stdlib.h.
#'
#' @param module LLVM module
#' @export
include_stdlib = function(module = Module())
{
  stdlib = list()

  atoi = Function("atoi", Int32Type, list(StringType), module = module)
  atof = Function("atof", DoubleType, list(StringType), module = module)

  stdlib$CONVERT_TO = c(
    "integer" = atoi,
    "factor" = atoi,
    "logical" = atoi,
    "numeric" = atof
  )

  return(stdlib)
}


#' Include stdio.h
#'
#' Declare structures and routines from stdio.h.
#'
#' @param module LLVM module
#' @export
include_stdio = function(module = Module())
{
  stdio = list()
  # Declare FILE structs.
  # We only ever need a FILE pointer (to the first element).
  FILEType = structType(list(o = Int32Type), "FILE")
  stdio$FILEPtrType = pointerType(FILEType)

  # Declare fopen(), fgets(), etc...
  stdio$fopen = 
    Function("fopen", stdio$FILEPtrType,
      paramTypes = list(StringType, StringType),
      module = module)
  stdio$fgets =
    Function("fgets", StringType, 
      paramTypes = list(StringType, Int32Type, stdio$FILEPtrType),
      module = module)
  stdio$fclose =
    Function("fclose", Int32Type,
      paramTypes = list(stdio$FILEPtrType),
      module = module)

  return(stdio)
}


#' Include Rdefines.h
#'
#' Declare structures and routines from Rdefines.h and Rinternals.h.
#'
#' @param module LLVM module
#' @export
include_rdefines = function(module = Module())
{
  rdefines = list()

  LOGICAL = Function("LOGICAL", Int32PtrType, list(SEXPType), module = module)
  INTEGER = Function("INTEGER", Int32PtrType, list(SEXPType), module = module)
  REAL = Function("REAL", DoublePtrType, list(SEXPType), module = module)
  R_CHAR = Function("R_CHAR", StringType, list(SEXPType), module = module)

  rdefines$POINTER_TO = c(
    "character" = R_CHAR,
    "integer" = INTEGER,
    "numeric" = REAL,
    "logical" = LOGICAL,
    "factor" = INTEGER
  )

  rdefines$SXPTYPE = c(
    "logical" = createIntegerConstant(10L),
    "integer" = createIntegerConstant(13L),
    "factor" = createIntegerConstant(13L),
    "numeric" = createIntegerConstant(14L),
    # STRSXP
    "character" = createIntegerConstant(16L),
    # VECSXP
    "list" = createIntegerConstant(19L)
  )

  rdefines$Rf_allocVector =
    Function("Rf_allocVector", SEXPType, list(Int32Type, Int32Type),
      module = module)

  rdefines$SET_STRING_ELT =
    Function("SET_STRING_ELT", VoidType, list(SEXPType, Int32Type, SEXPType),
      module = module)

  rdefines$Rf_protect =
    Function("Rf_protect", SEXPType, list(SEXPType), module = module)

  rdefines$Rf_unprotect =
    Function("Rf_unprotect", VoidType, list(Int32Type), module = module)

  rdefines$SET_VECTOR_ELT =
    Function("SET_VECTOR_ELT", SEXPType, list(SEXPType, Int32Type, SEXPType),
      module = module)

  rdefines$Rf_mkChar =
    Function("Rf_mkChar", SEXPType, list(StringType), module = module)

  return(rdefines)
}


#' Generate readCSV Routine
#'
#' Generate a routine in LLVM IR for reading a CSV file.
#'
#' @param colClasses column classes of CSV file
#' @param module LLVM module
#' @export
generate_readCSV = function(colClasses, module = Module())
{
  # Definitions --------------------------------------------------
  stdlib = include_stdlib(module)
  stdio = include_stdio(module)
  rdefines = include_rdefines(module)
  nextToken =
    Function("nextToken", StringType,
      paramTypes = list(stdio$FILEPtrType, Int32PtrType), module = module)

  types = list(r_file = StringType, r_n = Int32Type)
  fun = Function("readCSV", SEXPType, paramTypes = types, module = module)
  args = getParameters(fun)

  # Basic Blocks & Globals --------------------------------------------------
  b_entry = Block(fun, id = "entry")
  b_while_condition = Block(fun, id = "while_condition")
  b_while_body = Block(fun, id = "while_body")
  b_exit = Block(fun, id = "exit")
  builder = IRBuilder(b_entry)

  g_READ = createGlobalString(builder, "r", "READ")

  # Open File --------------------------------------------------
  ll_file =
    createCall(builder, stdio$fopen, args$r_file,
      createGEP(builder, g_READ, replicate(2, createIntegerConstant(0L))),
      id = "file")
  # TODO: Check that the file was actually opened.

  # Allocate Memory --------------------------------------------------
  # Allocate and protect each column.
  # TODO: Use names V1, V2, etc. like the C version.
  ll_col_sexps =
    lapply(colClasses,
      function(col) {
        ll_col_sexp =
          createCall(builder, rdefines$Rf_allocVector,
            rdefines$SXPTYPE[[col]], args$r_n)
        createCall(builder, rdefines$Rf_protect, ll_col_sexp)
        return(ll_col_sexp)
      })

  # Get pointer to underlying array for each column.
  ll_cols =
    mapply(
      function(col, ll_col_sexp) {
        if (col == "character") ll_col_sexp
        else createCall(builder, rdefines$POINTER_TO[[col]], ll_col_sexp)
      }, colClasses, ll_col_sexps, SIMPLIFY = FALSE)

  # Allocate loop counter.
  ll_i_ptr = createAlloc(builder, Int32Type, id = "i_ptr")
  createStore(builder, createIntegerConstant(0L), ll_i_ptr)

  # While Loop --------------------------------------------------
  createBr(builder, b_while_condition)
  setInsertBlock(builder, b_while_condition)

  # Load i.
  ll_i = createLoad(builder, ll_i_ptr, id = "i")
  tmp = createICmp(builder, ICMP_SLT, ll_i, args$r_n)
  createCondBr(builder, tmp, b_while_body, b_exit)

  # Body
  setInsertBlock(builder, b_while_body)

  # TODO: Each column should get its own basic block, so that NA can be
  # assigned for invalid values.
  mapply(
    function(col, ll_col) {
      # Call nextToken() once for each column.
      cur = createCall(builder, nextToken, ll_file,
        getNULLPointer(Int32PtrType))

      if (col == "character") {
        # Convert to SEXP.
        cur = createCall(builder, rdefines$Rf_mkChar, cur)

        # Store in variable.
        createCall(builder, rdefines$SET_STRING_ELT, ll_col, ll_i, cur)
      } else {
        # Convert to correct type.
        cur = createCall(builder, stdlib$CONVERT_TO[[col]], cur)

        # Store in variable.
        ptr = createGEP(builder, ll_col, ll_i)
        createStore(builder, cur, ptr)
      }
    }, colClasses, ll_cols, SIMPLIFY = FALSE)

  # Increment i.
  tmp = binOp(builder, Add, ll_i, createIntegerConstant(1L))
  createStore(builder, tmp, ll_i_ptr)
  
  createBr(builder, b_while_condition)

  # Cleanup --------------------------------------------------
  # Build a list and unprotect everything.
  setInsertBlock(builder, b_exit)

  createCall(builder, stdio$fclose, ll_file)

  ll_ans =
    createCall(builder, rdefines$Rf_allocVector,
      rdefines$SXPTYPE[["list"]], createIntegerConstant(length(colClasses)),
      id = "ans")
  createCall(builder, rdefines$Rf_protect, ll_ans)
  # Add columns to list.
  mapply(
    function(j, ll_col_sexp) {
      createCall(builder, rdefines$SET_VECTOR_ELT, ll_ans,
        createIntegerConstant(j), ll_col_sexp)
    }, seq_along(ll_col_sexps) - 1L, ll_col_sexps, SIMPLIFY = FALSE)

  createCall(builder, rdefines$Rf_unprotect, 
    createIntegerConstant(length(colClasses) + 1L))
  
  createRet(builder, ll_ans)

  return(module)
}
