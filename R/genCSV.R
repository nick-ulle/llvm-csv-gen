
# Generate LLVM IR to read a CSV file, given the column classes and file name.
# 1. Call fopen() on file.
# 2. Run a tokenizer in a while loop.
# 3. Put each token in its variable.

library(Rllvm)

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

include_stdio = function(module = Module())
  # Declare structures and functions from stdio.h.
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

include_rdefines = function(module = Module())
{
  rdefines = list()

  LOGICAL = Function("LOGICAL", Int32PtrType, list(SEXPType), module = module)
  INTEGER = Function("INTEGER", Int32PtrType, list(SEXPType), module = module)
  REAL = Function("REAL", DoublePtrType, list(SEXPType), module = module)
  #CHAR = Function("CHAR", StringType, list(SEXPType), module = module)

  rdefines$POINTER_TO = c(
  #  "character" = CHAR,
    "integer" = INTEGER,
    "numeric" = REAL,
    "logical" = LOGICAL,
    "factor" = INTEGER
  )

  # FIXME: Need to use Rf_allocVector
  #NEW_LOGICAL =
  #  Function("NEW_LOGICAL", SEXPType, list(Int32Type), module = module)
  #NEW_INTEGER =
  #  Function("NEW_INTEGER", SEXPType, list(Int32Type), module = module)
  #NEW_NUMERIC =
  #  Function("NEW_NUMERIC", SEXPType, list(Int32Type), module = module)
  #NEW_CHARACTER =
  #  Function("NEW_CHARACTER", SEXPType, list(Int32Type), module = module)

  #rdefines$ALLOCATE = c(
  #  "character" = NEW_CHARACTER,
  #  "integer" = NEW_INTEGER,
  #  "numeric" = NEW_NUMERIC,
  #  "logical" = NEW_LOGICAL,
  #  "factor" = NEW_INTEGER
  #)

  rdefines$STRING_ELT =
    Function("STRING_ELT", SEXPType, list(SEXPType, Int32Type),
      module = module)

  rdefines$PROTECT =
    Function("PROTECT", SEXPType, list(SEXPType), module = module)

  rdefines$UNPROTECT =
    Function("UNPROTECT", VoidType, list(Int32Type), module = module)

  #rdefines$NEW_LIST =
  #  Function("NEW_LIST", SEXPType, list(Int32Type), module = module)

  rdefines$SET_VECTOR_ELT =
    Function("SET_VECTOR_ELT", SEXPType, list(SEXPType, Int32Type, SEXPType),
      module = module)

  return(rdefines)
}

genCSV = function(colClasses = list("integer", "numeric", "integer"), module = Module())
  # Generate LLVM IR to read a CSV file.
{
  # Definitions --------------------------------------------------
  stdlib = include_stdlib(module)
  stdio = include_stdio(module)
  rdefines = include_rdefines(module)
  nextToken =
    Function("nextToken", StringType,
      paramTypes = list(stdio$FILEPtrType, Int32PtrType), module = module)

  types = list(r_file_vec = STRSXPType, r_n = INTSXPType)
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
  ll_file_elt =
    createCall(builder, rdefines$STRING_ELT, args$r_file_vec, 
      createIntegerConstant(0L), id = "file_elt")
  ll_file_name =
    createCall(builder, rdefines$POINTER_TO[["character"]], 
      ll_file_elt, id = "file_name")

  ll_file =
    createCall(builder, stdio$fopen, ll_file_name,
      createGEP(builder, g_READ, replicate(2, createIntegerConstant(0L))),
      id = "file")
  # TODO: Check that the file was actually opened.

  # Allocate and fill a buffer.
  #ll_buffer = createAlloc(builder, arrayType(Int8Type, 10000L), "buffer")
  #ll_buffer_ptr =
  #  createGEP(builder, ll_buffer, replicate(2, createIntegerConstant(0L)),
  #    "buffer_ptr")
  #createCall(builder, stdio$fgets, ll_buffer_ptr,
  #  createIntegerConstant(10000L), ll_file)

  # Allocate Memory --------------------------------------------------
  # Get a pointer to the number of rows.
  tmp = createCall(builder, rdefines$POINTER_TO[["integer"]], args$r_n)
  ll_n = createLoad(builder, tmp, id = "n")

  # Allocate and protect each column.
  # TODO: Use names V1, V2, etc. like the C version.
  ll_col_sexps =
    lapply(colClasses,
      function(col) {
        ll_col_sexp = createCall(builder, rdefines$ALLOCATE[[col]], ll_n)
        createCall(builder, rdefines$PROTECT, ll_col_sexp)
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
  tmp = createICmp(builder, ICMP_SLT, ll_i, ll_n)
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
        # Do special character stuff.
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

  ll_ans = createCall(builder, rdefines$NEW_LIST, ll_n, id = "ans")
  createCall(builder, rdefines$PROTECT, ll_ans)
  # Add columns to list.
  mapply(
    function(j, ll_col_sexp) {
      createCall(builder, rdefines$SET_VECTOR_ELT, ll_ans,
        createIntegerConstant(j), ll_col_sexp)
    }, seq_along(ll_col_sexps) - 1L, ll_col_sexps, SIMPLIFY = FALSE)

  createCall(builder, rdefines$UNPROTECT, 
    createIntegerConstant(length(colClasses) + 1L))
  
  createRet(builder, ll_ans)

  return(module)
}
