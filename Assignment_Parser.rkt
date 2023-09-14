#lang racket
;Vivek Venkata Sai Apuroop Thunuguntla
;16339238
; PARSE FUNCTOIN DEFINITION TAKES INPUT FILE AND START TOKENIZATION THEN CALL BACK FUNCTIONS ARE USED TO PARSE THE CODES.
(define (parse INPUT_FILE_NAME)
  (MAIN_FUNCTION (READ_INPUT_FILE INPUT_FILE_NAME)))


; FUNTION TO READ INPUT FILE AND SPLIT IT...
(define (READ_INPUT_FILE FILE_NAME) 
  (for/list ([INPUT_SPLITS (file->lines FILE_NAME)])
    (string-split INPUT_SPLITS)))


; FUNCTION TO TOKENIZE THE STRING AND TEST FOR CONDITIONAL STATEMENTS AND SYMBOLS USED IN THE CODE.
(define (TOKENIZER TOKENS_STR)
  (cond
    [(NUMBERIC_STRING? TOKENS_STR) (IDX_OR_NUMBER? TOKENS_STR)] 
    [(ALPHABETS_STRING? TOKENS_STR) 
      (cond
        [(equal? TOKENS_STR "if") 'if]
        [(equal? TOKENS_STR "then") 'then]
        [(equal? TOKENS_STR "goto") 'goto]
        [(equal? TOKENS_STR "gosub") 'gosub]
        [(equal? TOKENS_STR "return") 'return]
        [(equal? TOKENS_STR "read") 'read]
        [(equal? TOKENS_STR "write") 'write]
        [else 'id])]
    [else  
     (cond
        [(equal? TOKENS_STR ":") 'colon]
        [(equal? TOKENS_STR "(") 'lparen]
        [(equal? TOKENS_STR ")") 'rparen]
        [(equal? TOKENS_STR "$$") '$$]
        [(equal? TOKENS_STR "+") 'plus]
        [(equal? TOKENS_STR "-") 'minus]                 
        [(equal? TOKENS_STR "=") 'equals]                    
        [else  'err])]))




; MAIN FUNCTION DEFINITION
(define (MAIN_FUNCTION MF_LINE)
  (let ([OUTPUT_RESULTANT (FUNCTION_LINE_LIST MF_LINE)])
    (cond
      [(equal? OUTPUT_RESULTANT #t)            (println "Accepted")]
      [(equal? OUTPUT_RESULTANT 'MISSINGLINE)  (println "Missing line number")]
      [(equal? OUTPUT_RESULTANT 'MISSING$$)    (println "Missing $$ at end of line")]
      [else (printf "Syntax error at line ~a" OUTPUT_RESULTANT)])))



; FUNCTION TO TEST NUMERICAL STRING
(define (NUMBERIC_STRING? STRING) 
  (define (ITERATOR CHAR_LIST)
    (if (empty? CHAR_LIST)
        #t
        (if (char-numeric? (first CHAR_LIST))
            (ITERATOR (rest CHAR_LIST))
            #f)))
  (ITERATOR (string->list STRING)))


; FUNCTION TO TEST ALPHABETS STRINGS
(define (ALPHABETS_STRING? STRING) 
   (define (ITERATOR CHARACTER_LIST)
       (if (empty? CHARACTER_LIST)
          #t
          (if (char-alphabetic? (first CHARACTER_LIST))
              (ITERATOR (rest CHARACTER_LIST))
              #f)))
     (ITERATOR (string->list STRING)))




; FUNCTION TO TEST IDX
(define (IDX_OR_NUMBER? STRING) 
  (if (> (string->number STRING) 0)
      'idx 'num))


; FUNCTION TO CHECK LINES 
(define (FUNCTION_LINE FUN_LINE)
  (if (empty? FUN_LINE)
      #f 
      (let ([lineNumber (first FUN_LINE)])
        (let ([LINE_TOKEN (map TOKENIZER FUN_LINE)])
          (if (equal? (first LINE_TOKEN) 'idx)
              (CHECK_STATEMENT_FUNCTION (rest LINE_TOKEN))
              #f)))))


; FUNCTION TO CHECK LINE LIST
(define (FUNCTION_LINE_LIST FLL) 
  (cond
    [(empty? FLL)'MISSING$$]
    [(equal? (first (first FLL)) "$$") #t]
    [(number? (string->number (first (first FLL))))
        (if (list? (FUNCTION_LINE (first FLL))) (FUNCTION_LINE_LIST (rest FLL)) (first (first FLL)))]
    [else 'MISSINGLINE]))




; FUNCTION TO CHECK STATEMENTS
(define (CHECK_STATEMENT_FUNCTION CSF_LINE)
  (cond
    [(empty? CSF_LINE) #f] 
    [(equal? (first CSF_LINE) 'err)#f] 
    [(equal? (first CSF_LINE) 'id) (CHECK_EXPRESSION_FUNCTION (rest (rest CSF_LINE)))]  
    [(equal? (first CSF_LINE) 'if)(CHECK_STATEMENT_FUNCTION (rest (CHECK_EXPRESSION_FUNCTION (rest CSF_LINE))))]    
    [(equal? (first CSF_LINE) 'read)(rest (rest CSF_LINE))] 
    [(equal? (first CSF_LINE) 'write)(CHECK_EXPRESSION_FUNCTION (rest CSF_LINE))] 
    [(equal? (first CSF_LINE) 'goto) (rest (rest CSF_LINE))] 
    [(equal? (first CSF_LINE) 'gosub)(rest (rest CSF_LINE))] 
    [(equal? (first CSF_LINE) 'return)(rest CSF_LINE)]
    [else #f]))


; FUNCTION TO CHECK EXPRESSIONS
(define (CHECK_EXPRESSION_FUNCTION CEF_LINE)
  (cond
    [(empty? CEF_LINE)#f] 
    [(equal? (first CEF_LINE) 'err)#f] 
    [(equal? (first CEF_LINE) 'id)(EXPRESSION_TAIL_FUNCTION (rest CEF_LINE))] 
    [(equal? (first CEF_LINE) 'idx)(EXPRESSION_TAIL_FUNCTION (rest CEF_LINE))] 
    [(equal? (first CEF_LINE) 'num)(EXPRESSION_TAIL_FUNCTION (rest CEF_LINE))] 
    [(equal? (first CEF_LINE) 'lparen)(rest (CHECK_EXPRESSION_FUNCTION (rest CEF_LINE)))] 
    [else #f]))


; FUNCTION TO CHECK EXPRESSION TAIL
(define (EXPRESSION_TAIL_FUNCTION ETF_LINE)
  (cond
    [(empty? ETF_LINE)ETF_LINE] 
    [(equal? (first ETF_LINE) 'err)#f] 
    [(equal? (first ETF_LINE) 'plus)(CHECK_EXPRESSION_FUNCTION (rest ETF_LINE))]
    [(equal? (first ETF_LINE) 'minus)(CHECK_EXPRESSION_FUNCTION (rest ETF_LINE))]
    [(equal? (first ETF_LINE) 'equals)(CHECK_EXPRESSION_FUNCTION (rest ETF_LINE))]   
    [else ETF_LINE])) 


; TEST WITH GIVEN FILE 
(parse "file01.txt")
(parse "file02.txt")
(parse "file03.txt")
(parse "file04.txt")
(parse "file05.txt")
(parse "file06.txt")


