; ModuleID = 'read.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }

@.str = private unnamed_addr constant [8 x i8] c"foo.txt\00", align 1
@.str1 = private unnamed_addr constant [2 x i8] c"r\00", align 1
@.str2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8**, align 8
  %foo = alloca %struct._IO_FILE*, align 8
  %buffer = alloca [64 x i8], align 16
  store i32 %argc, i32* %1, align 4
  store i8** %argv, i8*** %2, align 8
  %3 = call %struct._IO_FILE* @fopen(i8* getelementptr inbounds ([8 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8]* @.str1, i32 0, i32 0))
  store %struct._IO_FILE* %3, %struct._IO_FILE** %foo, align 8
  %4 = getelementptr inbounds [64 x i8]* %buffer, i32 0, i32 0
  %5 = load %struct._IO_FILE** %foo, align 8
  %6 = call i8* @fgets(i8* %4, i32 10, %struct._IO_FILE* %5)
  %7 = getelementptr inbounds [64 x i8]* %buffer, i32 0, i32 0
  %8 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str2, i32 0, i32 0), i8* %7)
  %9 = load %struct._IO_FILE** %foo, align 8
  %10 = call i32 @fclose(%struct._IO_FILE* %9)
  ret i32 0
}

declare %struct._IO_FILE* @fopen(i8*, i8*) #1

declare i8* @fgets(i8*, i32, %struct._IO_FILE*) #1

declare i32 @printf(i8*, ...) #1

declare i32 @fclose(%struct._IO_FILE*) #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.1 (tags/RELEASE_351/final)"}
