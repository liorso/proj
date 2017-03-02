(load "pattern-matcher.scm")
(load "pc.scm")

;--------------------------------------------ass1------------------------------------------------

;************************Boolean**************************************

(define <Boolean>
  (new (*parser (word-ci "#t"))
       (*pack
	(lambda (_) #t))

       (*parser (word-ci "#f"))
       (*pack
	(lambda (_) #f))

       (*disj 2)
       done))

;****************************^<skipped*>***********************************

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))







;***************************Number************************************

(define <digit-0-9>
  (range #\0 #\9))


(define <digit-1-9>
  (range #\1 #\9))


(define <nat>
  (new 
       
       (*parser <digit-0-9>) *plus
       (*pack
	(lambda (s)
	  (string->number
	   (list->string
	    s))))

       done))


(define <int>
  (new 
       (*parser (char #\+))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
        (lambda (_ n)
                n))

       (*parser (char #\-))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
        (lambda (_ n)
                (* -1 n)))

       (*parser <nat>)

       (*disj 3)

       done))


(define <rat>
  (new (*parser <int>)
       (*parser (char #\/))
       (*parser <nat>)
       (*guard (lambda (n) (not (or (not n) (zero? n)))))
       (*caten 3)
       (*pack-with
	(lambda (num div den)
	  (/ num den)))
       done))


(define <SymbolNotAfterNumber>
        (new 
                (*parser <any-char>)
                (*parser <whitespace>)
                (*parser (char (integer->char 40)))
                (*parser (char (integer->char 41)))
                (*parser (char #\[ ))
                (*parser (char #\]))
                ;(*parser (char #\/))
                ;(*parser (char #\+))
                ;(*parser (char #\-))
                ;(*parser (char #\^))
                (*disj 5)

                *diff
        done))


          
       

(define <Number>
(new 
     (*parser <rat>)
     (*parser <int>)
     (*disj 2)

     (*parser <SymbolNotAfterNumber>)

     *not-followed-by
     done))






;***************************NumberInfix************************************



(define <SymbolNotAfterNumberInfix>
        (new 
                (*parser <any-char>)
                (*parser <whitespace>)
                (*parser (char (integer->char 40)))
                (*parser (char (integer->char 41)))
                (*parser (char #\[ ))
                (*parser (char #\]))
                (*parser (char #\/))
                (*parser (char #\+))
                (*parser (char #\-))
                (*parser (char #\^))
		(*parser (char #\,))
		(*parser (char #\*))
                (*disj 11)

                *diff
        done))


          
       

(define <NumberInfix>
(new 
     (*parser <rat>)
     (*parser <int>)
     (*disj 2)

     (*parser <SymbolNotAfterNumberInfix>)

     *not-followed-by
     done))












;***************************Char************************************

(define ^<meta-char>
  (lambda (str ch)
    (new 
        (*parser (word-ci str))
	(*pack (lambda (_) ch))
	done)))
     
(define <CharPrefix>
     (new 
          (*parser (char #\#))
          (*parser (char #\\))
          (*caten 2)
          (*pack-with
            (lambda (c1 c2)
                list->string `(,c1 ,c2)))
     done))
        
         
(define <VisbleSimpleChar>
    (new 
        (*parser <any-char>)
        (*parser (range (integer->char 0) (integer->char 32)))
        *diff
        (*pack (lambda (ch) ch))
        done))

(define <NamedChar>
    (new 
        (*parser (^<meta-char> "lambda" (integer->char 955)))
        (*parser (^<meta-char> "newline" #\newline))
        (*parser (^<meta-char> "nul" (integer->char 0)))
        (*parser (^<meta-char> "page" #\page)) ; formfeed
        (*parser (^<meta-char> "return" #\return))
        (*parser (^<meta-char> "space" (integer->char 32)))
        (*parser (^<meta-char> "tab" #\tab))
        
        (*disj 7)
        done))

(define <char-a-f>
    (new
        (*parser (range #\a #\f))
        (*parser (range #\A #\F))
        (*disj 2)
        done))
        

; (define <HexChar>
;     (new
;         (*parser <digit-0-9>)
;         (*parser <char-a-f>)
;         (*disj 2)
;         (*pack (lambda (ch) ch)) 
;         done))


(define <HexChar>
  (let ((zero (char->integer #\0))
	(lc-a (char->integer #\a))
	(uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
	 (*pack
	  (lambda (ch)
	    (- (char->integer ch) zero)))

	 (*parser (range #\a #\f))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) lc-a))))

	 (*parser (range #\A #\F))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) uc-a))))

	 (*disj 3)
	 done)))




(define (mul list) 
    (expt 16 (- (length list) 1)))

(define (getIntFromHexChars _ hex)
    (if (null? (cdr hex))
      (car hex)
      (+ (getIntFromHexChars _ (cdr hex)) (* (car hex) (mul hex)))))
	    

(define <HexUnicodeChar>
    (new
        (*parser (char-ci #\x))
        (*parser <HexChar>) *plus
        (*caten 2)
        (*pack-with
	        getIntFromHexChars)
        (*guard (lambda (n) (<= n 1114111)))
        (*pack
          (lambda (n) (integer->char n)))
        done))

;0x10FFFF

(define <Char> 
    (new 
        (*parser <CharPrefix>)
        

        (*parser <HexUnicodeChar>)

        (*parser <NamedChar>)

        (*parser <VisbleSimpleChar>)

        (*disj 3)
        (*caten 2)
        (*parser <SymbolNotAfterNumber>)
        *not-followed-by
        (*pack-with
            (lambda (x y) y))
        done))
















;****************************String***********************************

(define <string-meta-char>
    (new 
        (*parser (^<meta-char> "\\\\" #\\))
        (*parser (^<meta-char> "\\\"" #\"))
        (*parser (^<meta-char> "\\n" #\newline))
        (*parser (^<meta-char> "\\r" #\return))
        (*parser (^<meta-char> "\\t" #\tab))
        (*parser (^<meta-char> "\\f" #\page)) ; formfeed
        (*disj 6)
        done))



(define (getIntFromHexString _o hex _c)
    (if (null? (cdr hex))
      (car hex)
      (+ (getIntFromHexString _o (cdr hex) _c) (* (car hex) (mul hex)))))

(define <StringHexChar>
  (new
    (*parser (word-ci "\\x"))
    (*parser <HexChar>) *star
    (*parser (char #\;))
    (*caten 3)
    (*pack-with
      getIntFromHexString)
     (*guard (lambda (n) (<= n 1114111)))
        (*pack
          (lambda (n) (integer->char n)))
    done))

(define <string-char>
  (new
    (*parser <string-meta-char>)

    (*parser <any-char>) 
    (*parser (char #\"))
    (*parser (char #\\))
    (*disj 2)
    *diff
    
    (*parser <StringHexChar>)

    (*disj 3)
    done))


(define <String>
 (new (*parser (char #\"))
       (*parser <string-char>) *star
       (*parser (char #\"))
       (*caten 3)

       (*pack-with
	(lambda (open-delim chars close-delim)
	  (list->string chars)))

       done))













;****************************Symbol***********************************

(define <SymbolChar>
    (new 
        (*parser (range #\0 #\9))
        (*parser (range #\a #\z))
        (*parser (range #\A #\Z))
        (*parser (char #\!))
        (*parser (char #\$))
        (*parser (char #\^))
        (*parser (char #\*))
        (*parser (char #\-))
        (*parser (char #\_))
        (*parser (char #\=))
        (*parser (char #\+))
        (*parser (char #\<))
        (*parser (char #\>))
        (*parser (char #\?))
        (*parser (char #\/))
        (*disj 15)
        (*pack (lambda (ch) ch))
        done))

(define <Symbol>
(new
    (*parser <SymbolChar>) *plus
    (*pack
        (lambda (list)
          (string->symbol
                (string-downcase   
                 (list->string list)))))
    done))















;*****************************ProperList**********************************

(define <ProperList> 
    (new
        (*parser (char (integer->char 40) ))
        (*delayed (lambda () <Sexpr>)) *star
        (*parser (char (integer->char 41) ))
        (*caten 3)
        (*pack-with
            (lambda (opChar sexpr  closChar) 
                `(,@sexpr)))
        done))















;*****************************ImroperList**********************************

(define <ImproperList> 
    (new
        (*parser (char (integer->char 40) ))
        (*delayed (lambda () <Sexpr>)) *plus
        (*parser (char #\.))
        (*delayed (lambda () <Sexpr>))
        (*parser (char (integer->char 41) ))
        (*caten 5)
        (*pack-with
            (lambda (opChar sexprs dot sexpr closChar) 
                `(,@sexprs ,@sexpr)))
        done))















;*****************************Vector**********************************

(define <Vector> 
    (new
        (*parser (char #\# ))
        (*parser (char (integer->char 40) ))
        (*delayed (lambda () <Sexpr>)) *star
        (*parser (char (integer->char 41) ))
        (*caten 4)
        (*pack-with
            (lambda (letteron opChar sexprs closChar) 
                `#(,@sexprs)))
        done))










;*****************************Quoted**********************************

(define <Quoted> 
    (new
        (*parser (char #\' ))
        (*delayed (lambda () <Sexpr>))
        (*caten 2)
        (*pack-with
            (lambda (letteron  sexpr) 
                `(,'quote ,sexpr)))
        done))











;*****************************QuaziQuoted**********************************

(define <QuaziQuoted> 
    (new
        (*parser (char #\`))
        (*delayed (lambda () <Sexpr>))
        (*caten 2)
        (*pack-with
            (lambda (letteron  sexpr) 
                `(,'quasiquote ,sexpr)))
        done))












;*****************************Unquoted**********************************

(define <Unquoted> 
    (new
        (*parser (char #\, ))
        (*delayed (lambda () <Sexpr>))
        (*caten 2)
        (*pack-with
            (lambda (letteron  sexpr) 
                `(,'unquote ,sexpr)))
        done))













;*****************************UnquotedAndSpliced**********************************

(define <UnquotedAndSpliced> 
    (new
        (*parser (word ",@" ))
        (*delayed (lambda () <Sexpr>))
        (*caten 2)
        (*pack-with
            (lambda (letteron  sexpr) 
                `( ,'unquote-splicing ,sexpr)))
        done))

		
;**********************************Infix******************************************
(define <InfixSkip>
  (new (*parser (word "#;"))
	(*parser <whitespace>) *star
       (*delayed (lambda () <InfixExpression>))
       (*caten 3)
       done))
	


(define <PowerSymbol>
  (new (*parser (char #\^))
       (*parser (char #\*))
       (*parser (char #\*))
       (*caten 2)
       (*disj 2)
       done))

(define <SymbolInfixChar>
    (new 
        (*parser (range #\0 #\9))
        (*parser (range #\a #\z))
        (*parser (range #\A #\Z))
	(*pack (lambda (uper) (integer->char (- (char->integer uper) (- (char->integer #\A) (char->integer #\a))))))
        (*parser (char #\!))
        (*parser (char #\$))
        (*parser (char #\^))
        (*parser (char #\*))
        (*parser (char #\-))
        (*parser (char #\_))
        (*parser (char #\=))
        (*parser (char #\+))
        (*parser (char #\<))
        (*parser (char #\>))
        (*parser (char #\?))
        (*parser (char #\/))
        (*disj 15)
        (*pack (lambda (ch) ch))
        done))



(define <InfixPrefixExtensionPrefix>
    (new
      (*parser (word "##"))
      (*parser (word "#%"))
      (*disj 2)
      done))	

(define <Op>
	(new
		(*parser (char #\+))
		(*parser (char #\-))
		(*parser (char #\*))
		(*parser (char #\/))
		(*parser <PowerSymbol>)
		(*disj 5)
	done))	  		
				
(define <InfixSymbol>
		(new
			(*parser <SymbolInfixChar>) 
			(*parser <Op>)
			*diff
			*plus
	(*pack
        (lambda (list)
          (string->symbol  
            (list->string list))))
		done))	
(define <SymbolInfix> <InfixSymbol>)

(define intoVecRef 
  (lambda (into vec-ref)
    (if (list? vec-ref)
    (if (and (< 2 (length vec-ref)) (eq? 'vector-ref (car vec-ref)))
        `(vector-ref ,(intoVecRef into (cadr vec-ref)) ,(caddr vec-ref))
        `(vector-ref ,into ,(cadr vec-ref)))
    `(vector-ref ,into ,vec-ref)
        )))


(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

			
(define <InfixArgList>
	(new
		(*delayed (lambda() <t>))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser (char #\,))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*pack (lambda stam '()))
		(*delayed (lambda () <t>))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*pack (lambda stam '()))
		(*caten 4) *star
                (*pack-with (lambda expr (if (< 0 (length expr)) (map (lambda (a) (caddr a)) expr) '())))
                (*caten 3)
                (*pack-with (lambda (first _ param) (if (< 0 (length param)) (append `(,first) param) `(,first))))
                  
		(*parser <epsilon>)
		(*disj 2)
	done))

(define <squareOrpsik> 	
	(new

         (*parser (char #\[))
         (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
         (*delayed (lambda () <t>))
         (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
         (*parser (char #\]))
         (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
         (*delayed (lambda () <squareOrpsik>))
         ;(*parser <squareOrpsik>)
         (*caten 7)
         (*pack-with (lambda (_ _2 exp _3 _4 _1 exp1)
                       (if (and (< 2 (length exp1)) (eq? 'vector-ref (car exp1))) (intoVecRef exp exp1)
                       `(vector-ref ,`(vector-ref ,exp) ,(cadr exp1)))))
		                 
         (*parser (char #\[))
         (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
         (*delayed (lambda () <t>))
         (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
         (*parser (char #\]))
         (*caten 5)
         (*pack-with (lambda (_ _2 exp _3 _1)
                       `(vector-ref ,exp)))

         (*parser (char #\())
         (*parser <whitespace>)
	 (*parser <InfixSkip>)
	 (*disj 2) *star
         (*delayed (lambda () <InfixArgList>))
         (*parser <whitespace>)
	 (*parser <InfixSkip>)
	 (*disj 2) *star
	 (*parser (char #\)))
	 (*caten 5)
         (*pack-with (lambda (_ _2 exp _3 _1) 
                                     exp))

         (*disj 3)
	done))
    
(define <Escape>
	(new
	(*parser <InfixPrefixExtensionPrefix>)
        (*parser <whitespace>)
	(*parser <skip>)
	(*disj 2) *star
        (*delayed (lambda () <Sexpr>))
        (*caten 3)
        (*pack-with (lambda (_ _1 x) x))
done))
	

	
(define <End>
	(new 	
		(*parser (char #\-))
		(*parser <Escape>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <squareOrpsik>)
		(*caten 4)
                (*pack-with (lambda (_1 in _ exp)
			(if (not (pair? exp)) `(- ,`(,in))
                       (if (eq? 'vector-ref (car exp))
                       (if (< 2 (length exp)) `(- ,(intoVecRef in exp))
                       `(- ,`(vector-ref ,in ,(cadr exp))))
                       		`(- ,(append  `(,in) exp))))))


		(*parser (char #\-))
		(*parser <Escape>)
		(*caten 2)
                (*pack-with (lambda (_ esc) `(- ,esc)))

		(*parser <Escape>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <squareOrpsik>)
		(*caten 3)
                 (*pack-with (lambda (in _ exp)
			(if (not (pair? exp)) `(,in)
                       (if (eq? 'vector-ref (car exp))
                       (if (< 2 (length exp)) (intoVecRef in exp)
                       `(vector-ref ,in ,(cadr exp)))
				(if (= (length exp) 1) `(,in ,(car exp))
                       			(append `(,in) exp))))))	
		




		(*parser <Escape>)

		;(*parser <Escape>)
                ;(*parser <whitespace>)
		;(*parser <InfixSkip>)
		;(*disj 2) *star
		;(*delayed (lambda () <t>))
		

		(*parser (char #\-))
		(*parser <NumberInfix>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <squareOrpsik>)
		(*caten 4)
                 (*pack-with (lambda (_1 in _ exp)
			(if (not (pair? exp)) `(,in)
                       (if (eq? 'vector-ref (car exp))
                       (if (< 2 (length exp)) (intoVecRef (- in) exp)
                       `(vector-ref ,(- in) ,(cadr exp)))
                       (append `(,(- in)) exp)))))


		
		(*parser (char #\-))
		(*parser <NumberInfix>)
		(*caten 2)
                (*pack-with (lambda (_ num) (- num)))

		(*parser (char #\-))
		(*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <NumberInfix>)
		(*caten 3)
                (*pack-with (lambda (_ _1 num) `(- ,num)))

		(*parser (char #\-))
               	(*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <SymbolInfix>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <squareOrpsik>)
		(*caten 5)
                (*pack-with (lambda (_1  _2 in _ exp)
			(if (not (pair? exp)) `(- ,`(,in))
                       (if (eq? 'vector-ref (car exp))
                       (if (< 2 (length exp)) `(- ,(intoVecRef in exp))
                       `(- ,`(vector-ref ,in ,(cadr exp))))
                       		`(- ,(append  `(,in) exp))))))

		(*parser (char #\-))
               	(*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <SymbolInfix>)
		(*caten 3)
                (*pack-with (lambda (_ _1 ex) `(- ,ex)))

		(*parser (char #\-))
               	(*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*delayed (lambda () <t>))
		(*caten 3)
                (*pack-with (lambda (_ _1 ex) `(- ,ex)))



		(*parser <NumberInfix>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <squareOrpsik>)
		(*caten 3)
                 (*pack-with (lambda (in _ exp)
			(if (not (pair? exp)) `(,in)
                       (if (eq? 'vector-ref (car exp))
                       (if (< 2 (length exp)) (intoVecRef in exp)
                       `(vector-ref ,in ,(cadr exp)))
				(if (= (length exp) 1) `(,in ,(car exp))
                       			(append `(,in) exp))))))
                              



		(*parser <NumberInfix>)

		(*parser <InfixSymbol>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <squareOrpsik>)
		(*caten 3)
                 (*pack-with (lambda (in _ exp)
			(if (not (pair? exp)) `(,in)
                       (if (eq? 'vector-ref (car exp))
                       (if (< 2 (length exp)) (intoVecRef in exp)
                       `(vector-ref ,in ,(cadr exp)))
				(if (= (length exp) 1) `(,in ,(car exp))
                       			(append `(,in) exp))))))

		(*parser <InfixSymbol>)

		
		(*parser (char #\())
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*delayed (lambda () <t>))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser (char #\)))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <squareOrpsik>)
		(*caten 7)
                (*pack-with (lambda (_ _2 in _3 _4 _5 exp)
                       (if (eq? 'vector-ref (car exp))
                       (if (< 2 (length exp)) (intoVecRef in exp)
                       `(vector-ref ,in ,(cadr exp)))
                       `(,in ,(car exp)))))

		(*parser (char #\())
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*delayed (lambda () <t>))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser (char #\)))
		(*caten 5)
                (*pack-with (lambda (_ _2 exp1 _3 _4)
                              exp1))
					
				 
		;(*parser (char #\[))
		;(*delayed (lambda () <t>))
		;(*parser (char #\]))
		;(*parser <squareOrpsik>)
		;(*caten 4)


		(*parser (char #\[))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*delayed (lambda () <t>))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser (char #\]))
		(*caten 5)
                (*pack-with (lambda (_ _2 exp _3 _1)
                              `(vector-ref ,exp)))


		(*parser (char #\())
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*delayed (lambda () <InfixArgList>))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser (char #\)))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <squareOrpsik>)
		(*caten 7)
                (*pack-with (lambda (_ _2 in _3 _4 _5 exp)
                       (if (eq? 'vector-ref (car exp))
                       (if (< 2 (length exp)) (intoVecRef in exp)
                       `(vector-ref ,in ,(cadr exp)))
                       (flatten `(,in ,exp)))))

		(*parser (char #\())
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*delayed (lambda () <InfixArgList>))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser (char #\)))
		(*caten 5)
                (*pack-with (lambda (one _ exp _1 two)
                              `(,one ,exp ,two)))

		(*disj 19)
		
	done))
	
(define <Neg>
	(new
		(*parser (char #\-))
		(*parser <End>)
                (*caten 2)
                (*pack-with (lambda (_ num) (- num)))
		(*parser (char #\-))
		(*parser (char #\[))
		(*caten 2)
		(*parser (char #\-))
		(*parser (char #\,))
		(*caten 2)
		(*disj 2)
		*diff
		(*parser <End>)
		(*disj 2)

	done))
(define <PowerSymbol>
	(new
		(*parser (char #\^))
		(*parser (word "**"))
		(*disj 2)
	done))

		
(define PowRet
  (lambda (num exp)
    (if (= 0 (length exp)) num
    (if (> 2 (length exp)) `(expt ,num ,(cadar exp))
    `(expt ,num ,(PowRet (cadar exp) (cdr exp)))))
    ))
(define <Pow>
	(new
		(*parser <End>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <PowerSymbol>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*delayed (lambda () <Pow>))
		(*caten 3)
                (*pack-with
                 (lambda (_1 _ exp) `(expt ,exp)))
		(*parser <PowerSymbol>)
		(*parser (char #\[))
		(*caten 2)
		(*parser <PowerSymbol>)
		(*parser (char #\,))
		(*caten 2)
		(*disj 2)
		 *diff
		*star
		(*caten 3)
                (*pack-with
                 (lambda (num _ exp) (PowRet num exp)))
		
	done))

(define DivRet
  (lambda (num exp)
    (if (= 0 (length exp)) num
        (if (> 2 (length exp)) `(/ ,num ,(cadar exp))
          (DivRet `(/ ,num ,(cadar exp)) (cdr exp))))
    ))

(define <InfixDiv>
	(new
		(*parser <Pow>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser (char #\/))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <Pow>)
		(*caten 3)
                (*pack-with
                 (lambda (_1 _ exp) `(/ ,exp))) 
		(*parser (char #\/))
		(*parser (char #\[))
		(*caten 2)
		(*parser (char #\/))
		(*parser (char #\,))
		(*caten 2)
		(*disj 2)
		*diff
		*star
		(*caten 3)
                (*pack-with
                 (lambda (num _ exp) (DivRet num exp)))

	done))

(define checkpriorityMul
  (lambda (leftSym rightSym num exp)
    (if (pair? exp)
        (if (eqv? '/ (car exp)) `(/ ,(checkpriorityMul leftSym rightSym num (cadr exp)) ,(caddr exp))
           `(* ,num ,exp) )
        `(* ,num ,exp))))

(define MulRet
  (lambda (num exp)
    (if (= 0 (length exp)) num
        (if (> 2 (length exp)) (checkpriorityMul * / num (cadar exp))
          (MulRet (checkpriorityMul * / num (cadar exp)) (cdr exp))))
    ))	

(define <InfixMul>
	(new
		(*parser <InfixDiv>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser (char #\*))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <InfixDiv>)
		(*caten 3)
                (*pack-with
                 (lambda (_1 _ exp) `(* ,exp))) 
		(*parser (char #\*))
		(*parser (char #\[))
		(*caten 2)
		(*parser (char #\*))
		(*parser (char #\,))
		(*caten 2)
		(*disj 2)
		*diff	
		*star
		(*caten 3)
                (*pack-with
                 (lambda (num _ exp) (MulRet num exp)))
	done))

(define SubRet
  (lambda (num exp)
    (if (= 0 (length exp)) num
        (if (> 2 (length exp)) `(- ,num ,(cadar exp))

          (SubRet `(- ,num ,(cadar exp)) (cdr exp))))
    ))
	
		
(define <InfixSub>
	(new
		;(*parser (char #\-))
                ;(*parser <whitespace>)
		;(*parser <InfixSkip>)
		;(*disj 2) *star
		;(*parser <InfixSub>)
		;(*caten 3)
                ;(*pack-with
                ; (lambda (_1 _ exp) `(- ,exp)))

		(*parser <InfixMul>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser (char #\-))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <InfixMul>)
		(*caten 3)
                (*pack-with
                 (lambda (_1 _ exp) `(- ,exp))) 
		*star
		(*caten 3)
                (*pack-with
                 (lambda (num _ exp) (SubRet num exp)))

				

		(*parser <InfixMul>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser (char #\-))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <InfixMul>)
		(*caten 3)
                (*pack-with
                 (lambda (_1 _ exp) `(- ,exp))) 
		(*parser (char #\-))
		(*parser (char #\[))
		(*caten 2)
		(*parser (char #\-))
		(*parser (char #\,))
		(*caten 2)
		(*disj 2)
		*diff
		*star
		(*caten 3)
                (*pack-with
                 (lambda (num _ exp) (SubRet num exp)))
		(*disj 2)

	done))

(define checkpriorityPlus
  (lambda (leftSym rightSym num exp)
    (if (pair? exp)
        (if (eqv? '-  (car exp)) `(- ,(checkpriorityPlus leftSym rightSym num (cadr exp)) ,(caddr exp))
            `(+ ,num ,exp))
        `(+ ,num ,exp))))

(define AddRet
  (lambda (num exp)
    (if (= 0 (length exp)) num
        (if (> 2 (length exp)) (checkpriorityPlus + - num (cadar exp))
          (AddRet (checkpriorityPlus + - num (cadar exp)) (cdr exp))))
    ))		
			
(define <InfixAdd>
	(new
		(*parser (char #\+))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*delayed (lambda () <InfixAdd>))
		(*caten 3)
                (*pack-with
                 (lambda (_1 _ exp) exp))



		(*parser <InfixSub>)
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star

		(*parser (char #\+))
                (*parser <whitespace>)
		(*parser <InfixSkip>)
		(*disj 2) *star
		(*parser <InfixSub>)
		(*caten 3)
                (*pack-with
                 (lambda (_1 _ exp) `(+ ,exp)))
		(*parser (char #\+))
		(*parser (char #\[))
		(*caten 2)
		(*parser (char #\+))
		(*parser (char #\,))
		(*caten 2)
		(*disj 2)
		*diff
		*star
		(*caten 3)
                (*pack-with
                 (lambda (num _ exp) (AddRet num exp)))
		(*disj 2)			
	done))		



(define <t>
  (new  
	;(*parser <InfixAdd>)
        ;(*parser <InfixPrefixExtensionPrefix>)
        ;(*parser <whitespace>)
	;	(*parser <skip>)
	;	(*disj 2) *star
        ;(*delayed (lambda () <Sexpr>))
        ;(*caten 4)
        ;(*pack-with (lambda (c _ _1 x) `(,c ,x)))


	;(*parser <InfixPrefixExtensionPrefix>)
        ;(*parser <whitespace>)
	;	(*parser <skip>)
	;	(*disj 2) *star
        ;(*delayed (lambda () <Sexpr>))
        ;(*caten 3)
        ;(*pack-with (lambda (_ _1 x) x))

	(*parser <InfixAdd>)
        (*parser (char #\[))
        (*parser <InfixAdd>)
        (*parser (char #\]))
        (*caten 3)
        (*parser (char #\())
        (*parser <InfixAdd>)
        (*parser (char #\,))
        (*parser <InfixAdd>)
        (*parser (char #\)))
        (*caten 5)
        (*disj 2)
        *diff
        ;(*disj 2)
    done))

(define <InfixExpression> <t>)

(define <InfixExtension>
  (new	

	;(*parser <InfixPrefixExtensionPrefix>)
	;(*parser <whitespace>)
	;(*parser <InfixSkip>)
	;(*disj 2) *star
	;(*delayed (lambda () <InfixExtension>))
	;(*caten 3)
	;(*pack-with (lambda (_ _1 exp) (display exp) exp))
	

	(*parser <InfixPrefixExtensionPrefix>)
       (*parser <whitespace>)
	(*parser <InfixSkip>)
	(*disj 2) *star
        (*parser <InfixExpression>)
        (*caten 3) ;*plus
        (*pack-with (lambda (_ _1 exp) exp))
	;(*disj 2) *plus
	;(*pack (lambda (a) (car a)))
        done))









;*****************************Sexpr**********************************

(define <Sexpr>
    (new
        (*parser (^<skipped*>  
          (disj <Boolean>)))
        (*parser (^<skipped*>  
          (disj <Char>)))
        (*parser (^<skipped*>  
          (disj <Number>)))
        (*parser (^<skipped*>  
          (disj <String>)))
        (*parser (^<skipped*>  
          (disj <Symbol>)))
        (*parser (^<skipped*>  
          (disj <ProperList>)))
        (*parser (^<skipped*>  
          (disj <ImproperList>)))
        (*parser (^<skipped*>  
          (disj <Vector>)))
        (*parser (^<skipped*>  
          (disj <Quoted>)))
        (*parser (^<skipped*>  
          (disj <QuaziQuoted>)))
        (*parser (^<skipped*>  
          (disj <Unquoted>)))
        (*parser (^<skipped*>  
          (disj <UnquotedAndSpliced>)))   
        (*parser (^<skipped*>  
          (disj <InfixExtension>))) 
        (*disj 13)
  done))

(define <sexpr> <Sexpr>)

;-------------------------------------ass2

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
           (eq? (car e) tag)
           (pair? (cdr e))
           (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
         (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
                 simple-sexprs-predicates)
          (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
            (pair? e)
            (symbol? e)
            (vector? e))
        `',e
        e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
        (cadr e)
        e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
         (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
            (lambda (e)
              (cond ((unquote? e) (cadr e))
                    ((unquote-splicing? e)
                     (error 'expand-qq
                            "unquote-splicing here makes no sense!"))
                    ((pair? e)
                     (let ((a (car e))
                           (b (cdr e)))
                       (cond ((unquote-splicing? a)
                              `(append ,(cadr a) ,(expand-qq b)))
                             ((unquote-splicing? b)
                              `(cons ,(expand-qq a) ,(cadr b)))
                             (else `(cons ,(expand-qq a) ,(expand-qq b))))))
                    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
                    ((or (null? e) (symbol? e)) `',e)
                    (else e))))
           (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
           (optimizer
            (compose-patterns
             (pattern-rule
              `(append ,(? 'e) '())
              (lambda (e) (optimize-qq-expansion e)))
             (pattern-rule
              `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
              (lambda (c1 c2 e)
                (let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
                      (e (optimize-qq-expansion e)))
                  (optimize-qq-expansion `(append ,c ,e)))))
             (pattern-rule
              `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
              (lambda (c1 c2)
                (let ((c (quotify (append (unquotify c1) (unquotify c2)))))
                  c)))
             (pattern-rule
              `(append ,(? 'e1) ,(? 'e2))
              (lambda (e1 e2)
                (let ((e1 (optimize-qq-expansion e1))
                      (e2 (optimize-qq-expansion e2)))
                  `(append ,e1 ,e2))))
             (pattern-rule
              `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
              (lambda (c1 c2 e)
                (let ((c (quotify (list (unquotify c1) (unquotify c2))))
                      (e (optimize-qq-expansion e)))
                  (optimize-qq-expansion `(append ,c ,e)))))
             (pattern-rule
              `(cons ,(? 'e1) ,(? 'e2))
              (lambda (e1 e2)
                (let ((e1 (optimize-qq-expansion e1))
                      (e2 (optimize-qq-expansion e2)))
                  (if (and (const? e1) (const? e2))
                      (quotify (cons (unquotify e1) (unquotify e2)))
                      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))



;------------------------------------------------------------------------------------------------------
(define void-object
  (if #f #f))

;-------var----
(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))

(define reserved-word?
  (lambda (v) (ormap (lambda (x) (equal? v x)) *reserved-words*)
    ))

(define var?
  (lambda (v)
    (and (symbol? v) (not (reserved-word? v)))
    ))


;-----seq-----
(define seq-delete
  (lambda (seq)
    (if (null? seq) seq
        (if (equal? 'seq (car seq)) (seq-delete (cdr seq))
            (if (pair? (car seq)) (append (seq-delete (car seq)) (seq-delete (cdr seq)))
                 (list (car seq) (list (seq-delete (cdr seq)))))))))

(define pre-seq-delete
  (lambda (seq)
    (if (equal? 'seq (caadr seq)) `(seq ,@(seq-delete (cdr seq)))
        `(seq ,(seq-delete (cdr seq))))))
    




(define identify-lambda
  (lambda (argl ret-simple ret-opt ret-var)
    (cond 
      ((null? argl) (ret-simple '()))
      ((var? argl) (ret-var argl))     
      (else (identify-lambda (cdr argl)
                             (lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
                             (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
                             (lambda (var) (ret-opt `(,(car argl)) var)))))))



;---------------------------------------unbeginigy----------------------------------------


(define beginify
	(lambda (s)
		(cond
			((null? s) *void-object*)
			((null? (cdr s)) (cdr s))
			(else `(begin ,@s)))))

(define unbeginify ;;original unbeginify
  (lambda (s)
    (if (null? s)
        s
        (if (pair? s)
            (if (list? (car s))
                (if (eqv? 'begin (caar s))
                    `(,@(unbeginify(list-tail (car s) 1)) ,@(unbeginify(cdr s)))
                    `(,(car s) ,@(unbeginify (cdr s))))
                `(,(car s) ,@(unbeginify (cdr s))))
            s))))

(define parse
  (let ((run
         (compose-patterns

          ;--------------------applications-----------implimented
          (pattern-rule
           `(,(? 'proc (lambda (x) (not (reserved-word? x)))) . ,(? 'args)) ;maybe should change to reserved-symbol??
           (lambda (proc args)
             `(applic ,(parse proc) ,(map parse args))))
          ;---------------------const---------------implimented 
          ;Nil---------------implimented
          (pattern-rule
           (? 'c null?)
           (lambda (c) `(const '())))

          ;void---------------implimented
          (pattern-rule
           (? 'c (lambda (x) (equal? x void-object)))
           (lambda (c) `(const ,c)))
          ;vector---------------implimented
          (pattern-rule
           (? 'c vector?)
           (lambda (c) `(const ,c)))

          ;quote---------------implimented
          (pattern-rule
           `(quote ,(? 'c))
           (lambda (c) `(const ,c)))

          ;Boolean--------------implimented
          (pattern-rule
           (? 'c boolean?)
           (lambda (c) `(const ,c)))

          ;---------------------char--------------implimented
          (pattern-rule
           (? 'c char?)
           (lambda (c) `(const ,c)))

          ;---------------------number--------------implimented
          (pattern-rule
           (? 'c number?)
           (lambda (c) `(const ,c)))

          ;---------------------string--------------implimented
          (pattern-rule
           (? 'c string?)
           (lambda (c) `(const ,c)))

          ;---------------------var-----------------implimented
          (pattern-rule
           (? 'v var?)
           (lambda (v) `(var ,v)))

          ;---------------------if-------------------implimented
          ;if2
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit))
           (lambda (test dit)
             `(if3 ,(parse test) ,(parse dit) (const ,void-object))))
          ;if3
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (test dit dif)
             `(if3 ,(parse test) ,(parse dit) ,(parse dif))))

          ;--------------------Disjunctions----------------implimented
          (pattern-rule
           `(or . ,(? 'exprs))
           (lambda (exprs)
             (if (> (length exprs) 1)
             `(or ,(map parse exprs))
             (if (= (length exprs) 1)
             `,(parse (car exprs))
             `,(parse `#f))
             )))



 ;          --------------------Lambda-NEW---------------implimented----daniel
         
          (pattern-rule
           `(lambda ,(? 'args ) . ,(? 'exprs))
           (lambda (args exprs)
             (let*
                 (;(parsed 3)
                  (parsed `(,@(map parse (unbeginify exprs))))
                  (defs (split parsed (lambda (d e) d)))
                  (exps (split parsed (lambda (d e) e)))
                  (app `(applic
                         (lambda-simple
                          (,@(map cadadr defs))
                          (seq (,@(map (lambda (def) `(set ,(cadr def) ,@(cddr def))) defs)
                                ,@exps)))
                         (,@(map (lambda(x) '(const #f)) defs)))))
               

               (if (= 0 (length defs))
                   (if (> (length exprs) 1)  
                       (identify-lambda args
                                        (lambda (s) `(lambda-simple ,s (seq ,parsed)))
                                        (lambda (s opt) `(lambda-opt ,s ,opt (seq ,parsed)))
                                        (lambda (var) `(lambda-var ,var (seq ,parsed))))
                       (identify-lambda args
                                        (lambda (s) `(lambda-simple ,s ,(parse (car exprs))))
                                        (lambda (s opt) `(lambda-opt ,s ,opt ,(parse (car exprs))))
                                        (lambda (var) `(lambda-var ,var ,(parse (car exprs))))))
                   (identify-lambda args
                                    (lambda (s) `(lambda-simple ,s ,app))
                                    (lambda (s opt) `(lambda-opt ,s ,opt ,app))
                                    (lambda (var) `(lambda-var ,var ,app)))))))


           ;--------------------Define----------------implimented
           ;regular define
           (pattern-rule
            `(define ,(? 'v (lambda (x) (not (pair? x)))) ,(? 'e))
            (lambda (v e)
              `(def ,`(var ,v) ,(parse e))))

           ;MIT-style define
           (pattern-rule
            `(define ,(? 'v pair?) . ,(? 'e))
            (lambda (v e)
              `(def ,`(var ,(car v)) ,(parse (append `(lambda ,(cdr v)) e))))) ;Didn't test waiting for lambda


           ;--------------------Assignments----------------implimented
           (pattern-rule
            `(set! ,(? 'v) ,(? 'e))
            (lambda (v e)
              `(set ,`(var ,v) ,(parse e))))




           ;--------------------Sequences-----------implimented
           (pattern-rule
            `(begin  . ,(? 'seqs))
            (lambda (seqs)
              (if (> (length seqs) 1)
                  `(seq ,(map parse (unbeginify seqs)))
                  (if (= (length seqs) 1)
                      `,(parse (car seqs))
                      `,(parse `,void-object)))))



          
           ;---------------------let----------------implimented
           (pattern-rule
            `(let ,(? 'def) . ,(? 'body))
            (lambda (def body)
              (parse `((lambda ,(map car def) ,@body) ,@(map cadr def)))))
          
           ;---------------------let*----------------implimented
           (pattern-rule
            `(let* ,(? 'def) . ,(? 'body))
            (lambda (def body)
              (cond 
                ((null? def) (parse `((lambda () (begin ,@body)))))
                ((null? (cdr def)) (parse `(,`(lambda (,(caar def)) (begin ,@body)) ,(cadar def))))
                (else (parse `((lambda (,(caar def)) (let* ,(cdr def) ,@body)) ,(cadar def)))))))


           ;---------------------letrec----------------implimented
           (pattern-rule
            `(letrec ,(? 'def) . ,(? 'body))
            (letrec ((make-letrec (lambda (syms vals)
                                    (if (null? syms) syms
                                        (if (null? (cdr syms)) `((set! ,(car syms) ,(car vals)))
                                            `(,`(set! ,(car syms) ,(car vals)) ,@(make-letrec (cdr syms) (cdr vals))))))))
                (lambda (def body)
                  (parse `((lambda ,(map car def) 
                               (begin ,@(make-letrec (map car def) (map cadr def)) ((lambda () ,@body)))) 
                               ,@(map (lambda (x) #f) def))))))

          
           ;---------------------and----------------implimented
           (pattern-rule
            `(and)
            (lambda ()
              (parse #t)))
           (pattern-rule
            `(and ,(? 'con))
            (lambda (con)
              (parse con)))
           (pattern-rule
            `(and ,(? 'con1) ,(? 'con2))
            (lambda (con1 con2)
              (parse `(if ,con1 ,con2 #f))))
           (pattern-rule
            `(and . ,(? 'conses))
            (lambda (conses)
              `(if3 ,(parse (car conses)) ,(parse `(and ,@(cdr conses))) ,(parse #f))))

        ;--------------------QQ------------------------------------

           (pattern-rule
            `(quasiquote . ,(? 'exprs ))
            (lambda (exprs)
              (parse (expand-qq (car exprs)))
              ))
           

           ;---------------------cond----------------not implimented 
           
           (pattern-rule
            `(cond ,(? 'onec (lambda (x) (andmap pair? x))))
            (lambda (onec)
              (set! first (car onec))
              (set! other (cdr onec))
              (cond
                ((and (pair? (car first)) (equal? 'else (caar first))) (parse (cadar first)))
                ((equal? 'else (car first)) (parse (cadr first)))
                ((or (not (pair? other)) (null? other)) (parse `(if ,(car first) (begin ,@(cdr first)))))           
                (else  (parse `(if ,(car first)  (begin ,@(cdr first)) ,(if (equal? 'else (caar other)) `(begin ,@(cdar other))
                                                                     `(cond ,other))))))))
           (pattern-rule
            `(cond ,(? 'first) . ,(? 'other))
            (lambda (first other)
              (cond
                ((and (pair? (car first)) (equal? 'else (caar first))) (parse (cadar first)))
                ((equal? 'else (car first)) (parse (cadr first)))
                ((or (not (pair? other)) (null? other)) (parse `(if ,(car first) (begin ,@(cdr first)))))           
                (else  (parse `(if ,(car first) (begin ,@(cdr first)) ,(if (equal? 'else (caar other)) `(begin ,@(cdar other))
                                                                     `(cond ,other))))))))
         

           )))
        (lambda (e)
          (run e
               (lambda ()
                 (error 'parse
                        (format 'yet e)))))))

;-----------------------------------------------ass3------------------------------------------------------



;----------------------------------3 Eliminating nested define-expressions--------------------------------

(define eliminate-nested-defines
  (lambda (x) x))

(define split
  (lambda (pes ret-ds-es)
    (if (null? pes) (ret-ds-es '() '())
        (split
         (cdr pes)
         (lambda (ds es)
           (cond ((eq? (caar pes) 'def)
                  (ret-ds-es (cons (car pes) ds) es))
                 ((eq? (caar pes) 'seq)
                  (split (cadar pes)
                         (lambda (ds1 es1)
                           (ret-ds-es (append ds1 ds)
                                      (append es1 es)))))
                 (else (ret-ds-es ds (cons (car pes) es)))))))))



;----------------------------------4 remove-applic-lambda-nil--------------------------------
(define nil-lambda?
  (lambda (e)
    (and (pair? e) (= 3 (length e)) (equal? 'lambda-simple (car e)) (null? (cadr e)))
    ))

(define remove-applic-lambda-nil
  (lambda (e)
    (cond ((or (not (pair? e)) (null? e)) e)
          ((and (equal? 'applic (car e)) (nil-lambda? (cadr e))) (remove-applic-lambda-nil (caddr (cadr e))))
          (else `(,(remove-applic-lambda-nil (car e)) ,@(remove-applic-lambda-nil (cdr e)))))
    ))

;---------------------------------5 Boxing of variables-----------------------------------------
;----finding boxing
(define deep-member-bound
  (lambda (var e)
    (cond ((equal? var e) #t)
          ((or (not (pair? e)) (null? e)) #f)
          ((equal? (car e) 'const) #f)
          ((member var e) #t)
          ((and (equal? 'lambda-simple (car e)) (member var (cadr e))) #f)
          ((and (equal? 'lambda-opt (car e)) (or (member var (cadr e)) (equal? var (caddr e)))) #f)
          ((and (equal? 'lambda-var (car e)) (equal? var (cadr e))) #f)
          (else (or (deep-member-bound var (car e)) (deep-member-bound var (cdr e)))))
    ))    


(define bounded?
  (lambda (e var)
    (cond ((or (not (pair? e)) (null? e)) #f)
          ((equal? 'lambda-simple (car e)) (and (deep-member-bound var (caddr e)) (not (member var (cadr e)))))
          ((equal? 'lambda-opt (car e)) (and (deep-member-bound var (cadddr e)) 
                                             (not (or (member var (cadr e)) (equal? var (caddr e))))))
          ((equal? 'lambda-var (car e)) (and (deep-member-bound var (caddr e)) (not (equal? var (cadr e)))))
          (else (or (bounded? (car e) var) (bounded? (cdr e) var))))
    ))


(define var-read?
  (lambda (e var)
    (cond ((equal? var e) #t)
          ((or (not (pair? e)) (null? e)) #f)
          ((equal? (car e) 'set) (var-read? (cddr e) var))
          ((and (equal? 'lambda-simple (car e)) (member var (cadr e))) #f)
          ((and (equal? 'lambda-opt (car e)) (or (member var (cadr e)) (equal? var (caddr e)))) #f)
          ((and (equal? 'lambda-var (car e)) (equal? var (cadr e))) #f)
          ((equal? (car e) 'const) #f)
          (else (or (var-read? (car e) var) (var-read? (cdr e) var))))
    ))


(define var-write?
  (lambda (e var)
    (cond ((or (not (pair? e)) (null? e)) #f)
          ((and (equal? 'lambda-simple (car e)) (member var (cadr e))) #f)
          ((and (equal? 'lambda-opt (car e)) (or (member var (cadr e)) (equal? var (caddr e)))) #f)
          ((and (equal? 'lambda-var (car e)) (equal? var (cadr e))) #f)
          ((and (equal? (car e) 'set) (> (length e) 1) (equal? (cadadr e) var)) #t)
          (else (or (var-write? (car e) var) (var-write? (cdr e) var))))
    ))

(define should-box?
  (lambda (e var)
    (and (var-write? e var) (var-read? e var) (bounded? e var))
    ))

;------handaling boxing
(define make-boxes
  (lambda (to-box)
    (map (lambda (x) `(set (var ,x) (box (var ,x)))) to-box)
    ))

(define delete
  (lambda (original-list to-delete)
    (cond ((null? original-list) (list))
          ((equal? (car original-list) to-delete) (delete (cdr original-list) to-delete))
          (else (append (list (car original-list)) (delete (cdr original-list) to-delete))))
    ))

(define delete-elements
  (lambda (original-list to-delete)
    (if (null? to-delete) original-list
        (delete-elements (delete original-list (car to-delete)) (cdr to-delete)))
    ))

(define box-in-body
  (lambda (e list-vars)
    (cond ((or (not (pair? e)) (null? e) (null? list-vars)) e)
          ((and (equal? (car e) 'var) (member (cadr e) list-vars)) `(box-get ,e))
          ((and (equal? (car e) 'set) (equal? (caadr e) 'var) (member (cadadr e) list-vars)) 
           `(box-set ,(cadr e) ,@(box-in-body (cddr e) list-vars)))
          ((equal? 'lambda-simple (car e))  
           `(,(car e) ,(cadr e) ,(box-in-body (caddr e) (delete-elements list-vars (cadr e)))))
          ((equal? 'lambda-opt (car e))
           `(,(car e) ,(cadr e) ,(caddr e) ,(box-in-body (cadddr e) (delete-elements list-vars (cons (caddr e) (cadr e))))))
          ((equal? 'lambda-var (car e))  
           `(,(car e) ,(cadr e) ,(box-in-body (caddr e) (delete-elements list-vars (list (cadr e))))))
          (else `(,(box-in-body (car e) list-vars) ,@(box-in-body (cdr e) list-vars))))
    ))


(define box-vars-simple
  (lambda (e)
    (set! list-to-box (filter (lambda (x)  (should-box? (cddr e) x)) (cadr e)))
    `(,(make-boxes list-to-box) ,(box-in-body (cddr e) list-to-box))
    ))

(define box-vars-opt
  (lambda (e)
    (set! list-to-box (filter (lambda (x)  (should-box? (cdddr e) x)) (cons (caddr e) (cadr e))))
    `(,(make-boxes list-to-box) ,(box-in-body (cdddr e) list-to-box))
    ))

(define box-vars-var
  (lambda (e)
    (if (should-box? (cddr e) (cadr e))
        `(,(make-boxes (list (cadr e))) ,(box-in-body (cddr e) (list (cadr e))))
        '(()()))
    )) 
              
(define box-set
  (lambda (e)
    (cond ((or (not (pair? e)) (null? e)) e)
          ((and (equal? 'lambda-simple (car e)) (< 2 (length e)) (not (null? (car (box-vars-simple e)))))
                  `(,(car e) ,(cadr e) (seq (,@(car (box-vars-simple e)) ,@(if (equal? 'seq (caaadr (box-vars-simple e)))
                                                                               (box-set (cadar (cadr (box-vars-simple e))))
                                                                               (box-set (cadr (box-vars-simple e)))))))
                  )
          ((and (equal? 'lambda-opt (car e)) (< 2 (length e)) (not (null? (car (box-vars-opt e)))))
                  `(,(car e) ,(cadr e) ,(caddr e) (seq (,@(car (box-vars-opt e)) ,@(if (equal? 'seq (caaadr (box-vars-opt e)))
                                                                               (box-set (cadar (cadr (box-vars-opt e))))
                                                                               (box-set (cadr (box-vars-opt e)))))))
                  )
          ((and (equal? 'lambda-var (car e)) (< 2 (length e)) (not (null? (car (box-vars-var e)))))
                  `(,(car e) ,(cadr e) (seq (,@(car (box-vars-var e)) ,@(if (equal? 'seq (caaadr (box-vars-var e)))
                                                                               (box-set (cadar (cadr (box-vars-var e))))
                                                                               (box-set (cadr (box-vars-var e)))))))
                  )
          (else `(,(box-set (car e)) ,@(box-set (cdr e)))))
    ))
       


;----------------------------6 Annotating Variables with their Lexical address---------------------
(define do-lex-bound
  (lambda (body var major minor)
    (cond ((or (not (pair? body)) (null? body)) body)
          ((and (equal? 'var (car body)) (equal? var (cadr body))) `(bvar ,(cadr body) ,major ,minor))
          ((equal? 'lambda-simple (car body)) (if (member var (cadr body)) body
                                    `(,(car body) ,(cadr body) ,@(do-lex-bound (cddr body) var (+ 1 major) minor))))
          ((equal? 'lambda-opt (car body)) (if (member var (cons (caddr body) (cadr body))) body
                                    `(,(car body) ,(cadr body) ,(caddr body) 
                                                  ,@(do-lex-bound (cdddr body) var (+ 1 major) minor))))
          ((equal? 'lambda-var (car body)) (if (equal? var (cadr body)) body
                                    `(,(car body) ,(cadr body) ,@(do-lex-bound (cddr body) var (+ 1 major) minor))))
          (else `(,(do-lex-bound (car body) var major minor) ,@(do-lex-bound (cdr body) var major minor))))
    ))

(define do-lex-parameter
  (lambda (body var minor)
    (cond ((or (not (pair? body)) (null? body)) body)
          ((and (equal? 'var (car body)) (equal? var (cadr body))) `(pvar ,(cadr body) ,minor))
          ((equal? 'lambda-simple (car body)) (if (member var (cadr body)) body
                                    `(,(car body) ,(cadr body) ,@(do-lex-bound (cddr body) var 0 minor))))
          ((equal? 'lambda-opt (car body)) (if (member var (cons (caddr body) (cadr body))) body
                                    `(,(car body) ,(cadr body) ,(caddr body)
                                                  ,@(do-lex-bound (cdddr body) var 0 minor))))
          ((equal? 'lambda-var (car body)) (if (equal? var (cadr body)) body
                                    `(,(car body) ,(cadr body) ,@(do-lex-bound (cddr body) var 0 minor))))
          (else `(,(do-lex-parameter (car body) var minor) ,@(do-lex-parameter (cdr body) var minor))))
    ))

(define do-lex
  (lambda (body vars minor)
    (if (null? vars) body
        (do-lex (do-lex-parameter body (car vars) minor) (cdr vars) (+ 1 minor)))
    ))

(define pe->lex-pe
  (lambda (e)
    (cond ((or (not (pair? e)) (null? e)) e)
          ((equal? 'var (car e)) `(fvar ,@(cdr e)))
          ((equal? 'lambda-simple (car e)) `(,(car e) ,(cadr e) ,(pe->lex-pe (do-lex (caddr e) (cadr e) 0))))
          ((equal? 'lambda-opt (car e)) 
           `(,(car e) ,(cadr e) ,(caddr e) ,(pe->lex-pe (do-lex (cadddr e) `(,@(cadr e) ,(caddr e)) 0))))
          ((equal? 'lambda-var (car e)) `(,(car e) ,(cadr e) ,(pe->lex-pe (do-lex (caddr e) (list (cadr e)) 0))))
          (else `(,(pe->lex-pe (car e)) ,@(pe->lex-pe (cdr e)))))
    ))

;-------------------------------7  Annotating tail calls----------------------------------------
(define prepare-seq-to-tail
  (lambda (e not-tail is-first?)
    (cond ((and is-first? (= 1 (length e))) `(,(list not-tail) ,e))
          ((null? (cdr e)) `(,not-tail ,e))
          (else (prepare-seq-to-tail (cdr e) (if is-first? `(,not-tail ,(car e)) `(,@not-tail ,(car e))) #f)))
    ))


(define annotate-tc 1)

(define annotate-tail
  (lambda (pe)
    (cond ((or (not (pair? pe)) (null? pe)) pe)
          ((equal? 'const (car pe)) pe)
          ((or (equal? 'var (car pe)) (equal? 'fvar (car pe)) (equal? 'pvar (car pe)) (equal? 'bvar (car pe))) pe)
          ((equal? 'lambda-simple (car pe)) `(,(car pe) ,(cadr pe) ,(annotate-tail (caddr pe))))
          ((equal? 'lambda-opt (car pe)) `(,(car pe) ,(cadr pe) ,(caddr pe) ,(annotate-tail (cadddr pe))))
          ((equal? 'lambda-var (car pe)) `(,(car pe) ,(cadr pe) ,(annotate-tail (caddr pe))))
          ((equal? 'if3 (car pe)) `(if3 ,(annotate-tc (cadr pe)) ,(annotate-tail (caddr pe)) ,(annotate-tail (cadddr pe))))
          ((equal? 'or (car pe)) `(or (,@(map annotate-tc (car (prepare-seq-to-tail (cdadr pe) (caadr pe) #t))) 
                                       ,@(annotate-tail (cadr (prepare-seq-to-tail (cdadr pe) (caadr pe) #t))))))          
          ((or (equal? 'def (car pe)) (equal? 'define (car pe))) `(,(car pe) ,(cadr pe) ,(annotate-tc (caddr pe))))
          ((or (equal? 'box-set (car pe)) (equal? 'set (car pe))) `(,(car pe) ,(cadr pe) ,@(annotate-tc (cddr pe))))
          ((equal? 'box-get (car pe)) pe)
          ((equal? 'box (car pe)) `(box ,@(annotate-tc (cdr pe))))
          ((equal? 'applic (car pe)) `(tc-applic ,(annotate-tc (cadr pe)) ,(map annotate-tc (caddr pe))))
          ((equal? 'seq (car pe)) `(seq ,`(,@(map annotate-tc (car (prepare-seq-to-tail (cdadr pe) (caadr pe) #t)))
                                                 ,@(annotate-tail (cadr (prepare-seq-to-tail (cdadr pe) (caadr pe) #t))))))
          (else `(,(annotate-tail (car pe)))))
    ))


(define annotate-tc
  (lambda (pe)
    (cond ((or (not (pair? pe)) (null? pe)) pe)
          ((equal? 'const (car pe)) pe)
          ((or (equal? 'var (car pe)) (equal? 'fvar (car pe)) (equal? 'pvar (car pe)) (equal? 'bvar (car pe))) pe)
          ((equal? 'lambda-simple (car pe)) `(,(car pe) ,(cadr pe) ,(annotate-tail (caddr pe))))
          ((equal? 'lambda-opt (car pe)) `(,(car pe) ,(cadr pe) ,(caddr pe) ,(annotate-tail (cadddr pe))))
          ((equal? 'lambda-var (car pe)) `(,(car pe) ,(cadr pe) ,(annotate-tail (caddr pe))))
          ((equal? 'if3 (car pe)) `(if3 ,(annotate-tc (cadr pe)) ,(annotate-tc (caddr pe)) ,(annotate-tc (cadddr pe))))
          ((equal? 'or (car pe)) `(or ,(map annotate-tc (cadr pe))))
          ((or (equal? 'def (car pe)) (equal? 'define (car pe))) `(,(car pe) ,(cadr pe) ,(annotate-tc (caddr pe))))
          ((or  (equal? 'box-set (car pe)) (equal? 'set (car pe))) `(,(car pe) ,(cadr pe) ,@(annotate-tc (cddr pe))))
          ((equal? 'box-get (car pe)) pe)
          ((equal? 'box (car pe)) `(box ,@(annotate-tc (cdr pe))))
          ((equal? 'applic (car pe)) `(applic ,(annotate-tc (cadr pe)) ,(map annotate-tc (caddr pe))))
          ((equal? 'seq (car pe)) `(seq ,(map annotate-tc (cadr pe))))
          (else `(,(annotate-tc (car pe)))))
    ))   

;-----------------------------------------------------------------------------------------------------
;-------------------------------------Project---------------------------------------------------------
;-----------------------------------------------------------------------------------------------------

;--------------------------------------------General tools--------------------------
(define CODE "")
(define sa string-append)
(define ns number->string)
(define boolean->string
  (lambda (bool-val)
    (if bool-val
        "1"
        "0")))


(define map-in-order
  (lambda (proc lst)
    (letrec ((run (lambda (new old)
                    (if (null? old) new
                        (run (append new (list (proc (car old)))) (cdr old))))))
      (run '() lst))
    ))

             
;ltc - line to code
(define ltc
  (lambda (toadd)
    (set! CODE (sa CODE toadd ";" (string #\newline)))
    ))

;labtc - labale to code
(define labtc
  (lambda (lab)
    (set! CODE (sa CODE lab ":" (string #\newline)))
    ))


(define lab-construct
  (let ((index (box 0)))
    (lambda (s)
      (begin (set-box! index (+ 1 (unbox index)))
             (string-append s (number->string (unbox index))))
      )))

(define mov
  (lambda (to from)
    (sa "MOV(" to "," from ")")
    ))

(define imm
  (lambda (i)
    (sa "IMM(" i ")")
    ))

(define ind
  (lambda (add)
    (sa "IND(" add ")")
    ))

(define indd
  (lambda (add1 add2)
    (sa "INDD(" add1 "," add2 ")")
    ))

(define cmp
  (lambda (obj1 obj2)
    (sa "CMP(" obj1 "," obj2 ")")
    ))

(define jmp
  (lambda (lab)
    (sa "JUMP(" lab ")")
    ))

(define jmp-eq
  (lambda (lab)
    (sa "JUMP_EQ(" lab ")")
    ))

(define jmp-ne
  (lambda (lab)
    (sa "JUMP_NE(" lab ")")
    ))

(define jmp-le
  (lambda (lab)
    (sa "JUMP_LE(" lab ")")
    ))

(define jmp-ge
  (lambda (lab)
    (sa "JUMP_GE(" lab ")")
    ))

(define push
  (lambda (what)
    (sa "PUSH(" what ")")
    ))

(define call
  (lambda (proc)
    (sa "CALL(" proc ")")
    ))

(define calla
  (lambda (proc)
    (sa "CALLA(" proc ")")
    ))

(define drop
  (lambda (count)
    (sa "DROP(" count ")")
    ))

(define pop
  (lambda (reg)
    (sa "POP(" reg ")")
    ))

(define fparg
  (lambda (i)
    (sa "FPARG(" i ")")
    ))

(define stack
  (lambda (i)
    (sa "STACK(" i ")")
    ))

(define sub
  (lambda (dest src)
    (sa "SUB(" dest "," src ")")
    ))

(define add
  (lambda (dest src)
    (sa "ADD(" dest "," src ")")
    ))

(define div
  (lambda (dest src)
    (sa "DIV(" dest "," src ")")
    ))

(define mul
  (lambda (dest src)
    (sa "MUL(" dest "," src ")")
    ))

(define rem
  (lambda (dest src)
    (sa "REM(" dest "," src ")")
    ))

(define decr
  (lambda (dest)
    (sa "DECR(" dest ")")
    ))

(define incr
  (lambda (dest)
    (sa "INCR(" dest ")")
    ))

(define lab
  (lambda (l)
    (sa "LABEL(" l ")")
    ))

(define malloc
  (lambda (size to)
    (ltc (push size))
    (ltc (call "MALLOC"))
    (ltc (drop "1"))
    (ltc (mov to "R0"))
    ))

(define for
  (lambda (i do)
    (let ((exit-loop (lab-construct "FOR_EXIT_"))
          (start-loop (lab-construct "FOR_START_")))
      (ltc (push "R10"))
      (ltc (mov "R10" i))
      (labtc start-loop)
      (ltc (cmp "R10" "0"))
      (ltc (jmp-eq exit-loop))
      (ltc (push "R10"))
      (do)
      (ltc (pop "R10"))
      (ltc (decr "R10"))
      (ltc (jmp start-loop))
      (labtc exit-loop)
      (ltc (pop "R10"))  
    )))

;------------------------------------tables-------------------------------------------------------

;------------------------global-table
(define global-table (list))
(define next-free-global 0)

(define make-global-table-find
  (lambda (code)
    (cond ((or (not (pair? code)) (null? code)) '())
          ((equal? (car code) 'fvar) 
           (if (not (member (cadr code) (map cadr global-table)))
               (begin (set! global-table (cons (list next-free-global (cadr code)) global-table))
                      (set! next-free-global (+ 1 next-free-global)))))
          (else (begin (make-global-table-find (car code)) (make-global-table-find (cdr code)))))
    ))

(define make-global-table
  (lambda (code)
    (begin (make-global-table-find code)
           (set! global-table (sort (lambda (first sec) (< (car first) (car sec))) global-table))
           (malloc (ns next-free-global) "R15")
    )))

(define add-run-time-to-global
  (lambda (run-time-list)
    (map-in-order (lambda (x)
           (set! global-table (cons (list next-free-global x) global-table))
           (set! next-free-global (+ 1 next-free-global))) run-time-list)
    ))

(define lookup-global-help
  (lambda (key table)
    (cond ((null? table) "ERROR: NOT IN GLOBAL TABLE")
          ((equal? key (cadar table)) (caar table))
          (else (lookup-global-help key (cdr table))))
    ))

(define lookup-global
  (lambda (key)
    (number->string (lookup-global-help key global-table))
    ))

(define gen-fvar
  (lambda (pe)
    (ltc (push "R1"))
    (ltc (mov "R1" (lookup-global (cadr pe))))
    (ltc (add "R1" "R15"))
    (ltc (mov "R0" (ind "R1")))
    (ltc (pop "R1"))
    ))

(define set-fvar
  (lambda (pe)
    (code-gen (caddr pe))
    (ltc (push "R1"))
    (ltc (mov "R1" (lookup-global (cadr (cadr pe)))))
    (ltc (add "R1" "R15"))
    (ltc (mov (ind "R1") "R0"))
    (ltc (pop "R1"))
    (ltc (mov "R0" "T_VOID"))
    ))

(define gen-box-get-fvar
  (lambda (pe)
    (ltc (push "R1"))
    (ltc (mov "R1" (lookup-global (cadr pe))))
    (ltc (add "R1" "R15"))
    (ltc (mov "R0" (ind "R1")))
    (ltc (pop "R1"))
    (ltc (mov "R0" (ind "R0")))
    ))

(define def-fvar
  (lambda (pe)
    (code-gen (caddr pe))
    (ltc (push "R1"))
    (ltc (mov "R1" (lookup-global (cadr (cadr pe)))))
    (ltc (add "R1" "R15"))
    (ltc (mov (ind "R1") "R0"))
    (ltc (mov "R0" "T_VOID"))
    (ltc (pop "R1"))
    ))
  
 
  

;-----------------------const-table
(define const-table (list))

(define find-const-in-code
  (lambda (code)
    (cond ((or (not (pair? code)) (null? code)) '())
          ((equal? (car code) 'const) (cadr code))
          (else `((find-const-in-code (car code)) ,@(find-const-in-code (cdr code))))) ; NOT GOOD!
    ))

(define delete-dup
  (lambda (e)
    (if (or (null? e) (null? (cdr e))) e
        (if (member (car e) (cdr e)) (delete-dup (cdr e))
            (cons (car e) (delete-dup (cdr e)))))))

(define type-const
  (lambda (e)
    (cond
      ((or (char? e) (number? e) (string? e) (null? e) (boolean? e)) `(,e)); was void?
      ((pair? e)
       `(,e ,@(type-const (car e)) ,@(type-const (cdr e))))
       ((vector? e)
        `(,e ,@(apply append
                      (map type-const
                           (vector->list e)))))
       ((symbol? e)
        `(,e ,@(type-const (symbol->string e))))
       (else (type-const (cdr e)))
       )))

(define const-lookup
  (lambda (val const-table)
    (cond
      ((null? const-table) '())
      ((equal? val (caar const-table)) (cadar const-table))
      (else (const-lookup val (cdr const-table))))))

(define const-lookup-vector
  (lambda (const-table)
    (lambda (vector-ref)
      (const-lookup vector-ref const-table))))     


(define make-tagged-offset2 ; todo complete for all types
  (lambda (list_of_consts offset const-table)
    (if (null? list_of_consts)
        '()
        (let ((first (car list_of_consts))
              (remainder (cdr list_of_consts)))
          (cond
            ((null? first)
             (append const-table `(,first ,offset (T_nil)) (make-tagged-offset remainder (+ 1 offset) const-table)))
            ((number? first)
             (append const-table `(,first ,offset (T_int ,first)) (make-tagged-offset remainder (+ 2 offset) const-table))))
            ((pair? first)
             (append const-table `(,first ,offset (T_pair ,(const-lookup (car first) const-table) ,(const-lookup (cdr first) const-table))) (make-tagged-offset remainder (+ 3) const-table)))))))



      
(define make-tagged-offset ; todo complete for all types
  (lambda (const offset const-table)
    (begin
;      (display "raw-const:[")
;      (display const)
;      (display "offset:(")
;      (display offset)
;      (display ")")
;      (display "]\n")
      (if (null? const)
          const-table
          (let ((first (car const))
                (remainder (cdr const)))
            (cond ((null? first)
                   (make-tagged-offset remainder (+ 1 offset) (append const-table `((,first ,offset (T_nil))))))
                  ((equal? first void-object)
                   (make-tagged-offset remainder (+ 1 offset) (append const-table `((,first ,offset (T_void))))))
                  ((boolean? first)
                   (make-tagged-offset remainder (+ 2 offset) (append const-table `((,first ,offset (T_bool ,first))))))
                  ((and (number? first) (integer? first))
                   (make-tagged-offset remainder (+ 2 offset) (append const-table `((,first ,offset (T_integer ,first))))))
                  ((and (number? first) (not (integer? first)))
                   (make-tagged-offset remainder (+ 3 offset) (append const-table `((,first ,offset (T_fraction ,(numerator first) ,(denominator first)))))))
                  ((char? first)
                   (make-tagged-offset remainder (+ 2 offset) (append const-table `((,first ,offset (T_char ,first))))))
                  ((string? first)
                   (make-tagged-offset remainder (+ 2 (string-length first) offset) (append const-table `((,first ,offset (T_string ,(string-length first) ,@(string->list first)))))))
                  ((symbol? first)
                   (make-tagged-offset remainder (+ 2 offset) (append const-table `((,first ,offset (T_symbol  ,(const-lookup (symbol->string first) const-table)))))))
                  ((vector? first)
                   (make-tagged-offset remainder (+ 2 (vector-length first) offset) (append const-table `((,first ,offset (T_vector ,(vector-length first) ,@(vector->list (vector-map (const-lookup-vector const-table) first))))))))
                  ((pair? first)
                   (make-tagged-offset remainder (+ 3 offset) (append const-table `((,first ,offset (T_pair ,(const-lookup (car first) const-table) ,(const-lookup (cdr first) const-table)))))))))))))





(define  get-list-of-consts
  (lambda (code list-of-consts)
    (cond ((null? code) list-of-consts)
          ((list? (car code)) (append list-of-consts (get-list-of-consts (car code)(list)) (get-list-of-consts (cdr code) (list))))
          ((equal? (car code) 'const) (append list-of-consts (cdr code)))
          (else (append list-of-consts (get-list-of-consts (cdr code) (list)))))))




(define make-const-table-2
  (lambda (code memory-location)
    (let ((vector-of-consts (list->vector (get-list-of-consts code (list)))))
      (begin
        ;(display "code{")
        ;(display code)
        ;(display "}")
        ;        (display ":vector:")
        ;        (display vector-of-consts)
        ;        (display ":;\n")
        (set! const-table (reverse (cdr (reverse (make-tagged-offset (reverse (delete-dup (type-const vector-of-consts))) memory-location (list))))))
        ;(set! const-table (map-in-order map-make-tagged-offset-caller list-of-consts))
        ;(display (make-tagged-offset (reverse (delete-dup (type-const list-of-consts))) memory-location (list)))
        ;(display "ct[")
        ;(display const-table)
        ;(display "]\n")
        (gen-const-table const-table)
        const-table))))

(define make-const-table
  (lambda (code)
                (make-const-table-2
                 ;(append code `((const #t) (const #f) (const ()) (const ,void-object )))
                 (append  `((const #t #f ())) code)
                 ;code
                 const-table-mem-location)))

           
(define gen-const-table
  (lambda (const-table-local)
    (if (not (null? const-table-local)) 
        (begin
          ;(display "ct[")
          ;(display const-table-local)
          ;(display "]")
          (let* ((first (car const-table-local))
                 (rest (cdr const-table-local))
                 (type (caaddr first))
                 (value (reverse(cdr(caddr first)))))
            ;            (display "first: ")
            ;            (display first)
            ;            (display ";")
            ;            (display "type: ")
            ;            (display type)
            ;            (display ";")
            ;            (display "value: ")
            ;            (display value)
            ;            (display ";\n")
            (cond ((equal? type 'T_nil) (gen-make-sob-nil))
                  ((equal? type 'T_void) (gen-make-sob-void))
                  ((equal? type 'T_bool) (gen-make-sob-bool value))
                  ((equal? type 'T_integer) (gen-make-sob-integer value))
                  ((equal? type 'T_fraction) (gen-make-sob-fraction value))
                  ((equal? type 'T_char) (gen-make-sob-char value))
                  ((equal? type 'T_string) (gen-make-sob-string value))
                  ((equal? type 'T_symbol) (gen-make-sob-symbol value))
                  ((equal? type 'T_vector) (gen-make-sob-vector value))
                  ((equal? type 'T_pair) (gen-make-sob-pair value)))
            (gen-const-table rest))))))

    
(define const-table-mem-location 1)


;------------------------------------code gen-------------------------------------------------------------
(define code-gen 1)

(define gen-make-sob-nil
  (lambda ()
    ;(display "nil\n")
    (ltc (call "MAKE_SOB_NIL"))))

(define gen-make-sob-void
  (lambda ()
    ;(display "void\n")
    (ltc (call "MAKE_SOB_VOID"))))

(define gen-make-sob-bool
  (lambda (value)
    ;(display "bool\n")
    (if (car value)
        (begin (ltc (push (imm  (ns 1))))
               (ltc (call "MAKE_SOB_BOOL")))
        (begin (ltc (push (imm  (ns 0))))
               (ltc (call "MAKE_SOB_BOOL"))))))

(define gen-make-sob-integer
  (lambda (value)
    ;(display "int\n")
    (begin (ltc (push (imm  (ns (car value)))))
           (ltc (call "MAKE_SOB_INTEGER")))))

(define gen-make-sob-fraction
  (lambda (value)
    (begin (ltc (push (imm  (ns (cadr value)))))
           (ltc (push (imm  (ns (car value)))))
           (ltc (call "MAKE_SOB_FRACTION")))))


(define gen-make-sob-char
  (lambda (value)
    ;(display "char\n")
    ;(display value)
    (begin (ltc (push (imm  (ns(char->integer (car value))))))
           (ltc (call "MAKE_SOB_CHAR")))))

(define gen-make-sob-string
  (lambda (value)
    ;(display "string\n")
    (let ((reversed (reverse value)))
      (begin (map-in-order (lambda (x) (ltc (push (imm (ns(char->integer x)))))) (cdr reversed)) ;;string
             (ltc (push (imm  (ns (car reversed))))) ;;size
             (ltc (call "MAKE_SOB_STRING"))))))

(define gen-make-sob-symbol
  (lambda (value)
    ;(display "symbol\n")
    (begin (ltc (push (imm (ns (car value)))))  ;;string address
           (ltc (call "MAKE_SOB_SYMBOL")))))

(define gen-make-sob-vector
  (lambda (value)
    ;(display "vector\n")
    (let ((reversed (reverse value)))
    (begin  (map-in-order (lambda (x) (ltc (push (imm (ns x))))) (cdr reversed)) ;;list of addresses + size
            (ltc (push (imm  (ns (car reversed))))) ;;size
            (ltc (call "MAKE_SOB_VECTOR"))))))
 

(define gen-make-sob-pair
  (lambda (value)
    ;(display "pair\n")
    (begin (ltc (push (imm  (ns (car value)))))
           (ltc (push (imm  (ns (cadr value)))))
           (ltc (call "MAKE_SOB_PAIR")))))

(define gen-if3
    (lambda (pe)
      (begin (define lab-else (lab-construct "L_if3_else_"))
             (define lab-exit (lab-construct "L_if3_exit_"))
             (code-gen (cadr pe))
             (ltc (cmp "R0" (ns (const-lookup #f const-table))))
             (ltc (jmp-eq lab-else))
             (code-gen (caddr pe))
             (ltc (jmp lab-exit))
             (labtc lab-else)
             (code-gen (caddr (cdr pe)))
             (labtc lab-exit)
             )))
  
(define gen-or
    (lambda (pe)
      (begin (define lab-exit (lab-construct "L_or_exit_"))
             (map-in-order
              (lambda (l)
                (begin (code-gen l)
                       (ltc (cmp "R0" (ns (const-lookup #f const-table))))
                       (ltc (jmp-ne lab-exit)))) (cadr pe))
             (labtc lab-exit)
             )))


(define gen-applic
  (lambda (pe)
    (begin (map-in-order
              (lambda (l)
                (begin (code-gen l)
                       (ltc (push "R0"))))
                       (reverse (caddr pe)))
            (ltc (push (imm (ns (length (caddr pe))))))
            (code-gen (cadr pe))
            (ltc (cmp (indd "R0" "0") "T_CLOSURE"))
            (ltc (jmp-ne "NOT_CLOSURE"))
            (ltc (sa (push (indd "R0" "1")) "/*env*/"))
            (ltc (calla (indd "R0" "2")))
            (ltc (drop "1"))
            (ltc (pop "R1"))
            (ltc (drop "R1"))
            )))

(define major 0)

(define gen-lambda-simple
  (lambda (pe)
    (let ((body-leb (lab-construct "CLOS_BODY_"))
          (exit-clos-leb (lab-construct "EXIT_CLOS_")))
      (begin
        (ltc (mov "R1" (fparg "0")))
        (malloc (ns (+ 1 major)) "R2")
        (ltc (mov "R4" (ns 0)))
        (ltc (mov "R5" (ns 1)))
        (for (ns major)
          (lambda () (begin (ltc (mov "R6" (indd "R1" "R4")))
                            (ltc (mov (indd "R2" "R5") "R6"))
                            (ltc (incr "R4"))
                            (ltc (incr "R5")))))
        (ltc (mov "R3" (fparg "1")))
        (malloc "R3" (indd "R2" "0"))
        (ltc (mov "R4" (ns 0)))
        (ltc (mov "R5" (ns 2)))
        (ltc (mov "R7" (indd "R2" "0")))
        (for "R3"
          (lambda ()
            (begin (ltc (mov "R6" (fparg "R5")))
                   (ltc (mov (indd "R7" "R4") "R6"))
                   (ltc (incr "R5"))
                   (ltc (incr "R4")))))
        (malloc "3" "R0")
        (ltc (mov (indd "R0" "0") "T_CLOSURE"))
        (ltc (mov (indd "R0" "1") "R2"))
        (ltc (mov (indd "R0" "2") (lab body-leb)))
        (ltc (jmp exit-clos-leb))
        
        (labtc body-leb)
        (ltc (push "FP"))
        (ltc (mov "FP" "SP"))
        (ltc (cmp (fparg "1") (ns (length (cadr pe)))))
        (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
        (set! major (+ 1  major))
        (code-gen (caddr pe))
        (set! major (- major 1))
        (ltc (pop "FP"))
        (ltc "RETURN")
        (labtc exit-clos-leb)))
    ))

(define gen-lambda-opt
  (lambda (pe)
    (let ((body-leb (lab-construct "CLOS_BODY_OPT_"))
          (exit-clos-leb (lab-construct "EXIT_CLOS_OPT_")))
      (begin 
        (ltc (mov "R1" (fparg "0")))
        (malloc (ns (+ 1 major)) "R2")
        (ltc (mov "R4" (ns 0)))
        (ltc (mov "R5" (ns 1)))
        (for (ns major)
          (lambda () (begin (ltc (mov "R6" (indd "R1" "R4")))
                            (ltc (mov (indd "R2" "R5") "R6"))
                            (ltc (incr "R4"))
                            (ltc (incr "R5")))))
        (ltc (mov "R3" (fparg "1")))
        (malloc "R3" (indd "R2" "0"))
        (ltc (mov "R4" (ns 0)))
        (ltc (mov "R5" (ns 2)))
        (ltc (mov "R7" (indd "R2" "0")))
        (for "R3"
          (lambda ()
            (begin (ltc (mov "R6" (fparg "R5")))
                   (ltc (mov (indd "R7" "R4") "R6"))
                   (ltc (incr "R5"))
                   (ltc (incr "R4")))))
        (malloc "3" "R0")
        (ltc (mov (indd "R0" "0") "T_CLOSURE"))
        (ltc (mov (indd "R0" "1") "R2"))
        (ltc (mov (indd "R0" "2") (lab body-leb)))
        (ltc (jmp exit-clos-leb))
        
        (labtc body-leb)
        (ltc (push "FP"))
        (ltc (mov "FP" "SP"))
        ;Stack fix
        (let ((lab-exact-args (lab-construct "OPT_EXACT_ARGS_"))
              (lab-exit-stack-fix (lab-construct "EXIT_STACK_FIX_"))
              (list-length (length (cadr pe))))
          (begin (ltc (cmp (fparg "1") (ns list-length)))
                 (ltc (jmp-eq lab-exact-args))
                 (ltc (push "R1"))
                 (ltc (push "R2"))
                 (ltc (push "R3"))
                 (ltc (call "MAKE_SOB_NIL"))
                 (ltc (mov "R1" "R0"))
                 (ltc (mov "R2" (fparg "1")))
                 (ltc (sub "R2" (ns list-length)))
                 (ltc (mov "R3" (fparg "1")))
                 (ltc (add "R3" "1"))
                 (for "R2"
                   (lambda ()
                     (ltc (push "R1"))
                     (ltc (push (fparg "R3")))
                     (ltc (call "MAKE_SOB_PAIR"))
                     (ltc (drop "2"))
                     (ltc (mov "R1" "R0"))
                     (ltc (decr "R3"))))
                 (ltc (mov (fparg (ns (+ 2 list-length))) "R1"))
                 (ltc (pop "R3"))
                 (ltc (pop "R2"))
                 (ltc (pop "R1"))
                 (ltc (jmp lab-exit-stack-fix))

                 (labtc lab-exact-args)
                 (ltc (push "R2"))
                 (ltc (push "R3"))
                 (ltc (push "R4"))
                 (ltc (mov "R2" (ns (+ 4 list-length))))
                 (ltc (mov "R3" (ns -2)))
                 (for "R2"
                   (lambda () (begin (ltc (mov "R4" (fparg "R3")))
                                     (ltc (decr "R3"))
                                     (ltc (mov (fparg "R3") "R4"))
                                     (ltc (incr "R3"))
                                     (ltc (incr "R3"))
                                     )))
                 (ltc (incr "FP"))
                 (ltc (incr "SP"))
                 (ltc (mov "R2" (fparg "1")))
                 (ltc (incr "R2"))
                 (ltc (mov (fparg "1") "R2"))
                 (ltc (call "MAKE_SOB_NIL"))
                 (ltc (mov (fparg (ns (+ 2 list-length))) "R0"))
                 (ltc (pop "R4"))
                 (ltc (pop "R3"))
                 (ltc (pop "R2"))
                 (labtc lab-exit-stack-fix)
                 (set! major (+ 1  major))
                 (code-gen (cadddr pe))
                 (set! major (- 1 major))
                 (ltc (pop "FP"))
                 (ltc "RETURN")
                 (labtc exit-clos-leb)))
        ))))

(define gen-lambda-var
  (lambda (pe)
    (let ((body-leb (lab-construct "CLOS_BODY_VAR_"))
          (exit-clos-leb (lab-construct "EXIT_CLOS_VAR_")))
      (begin 
        (ltc (mov "R1" (fparg "0")))
        (malloc (ns (+ 1 major)) "R2")
        (ltc (mov "R4" (ns 0)))
        (ltc (mov "R5" (ns 1)))
        (for (ns major)
          (lambda () (begin (ltc (mov "R6" (indd "R1" "R4")))
                            (ltc (mov (indd "R2" "R5") "R6"))
                            (ltc (incr "R4"))
                            (ltc (incr "R5")))))
        (ltc (mov "R3" (fparg "1")))
        (malloc "R3" (indd "R2" "0"))
        (ltc (mov "R4" (ns 0)))
        (ltc (mov "R5" (ns 2)))
        (ltc (mov "R7" (indd "R2" "0")))
        (for "R3"
          (lambda ()
            (begin (ltc (mov "R6" (fparg "R5")))
                   (ltc (mov (indd "R7" "R4") "R6"))
                   (ltc (incr "R5"))
                   (ltc (incr "R4")))))
        (malloc "3" "R0")
        (ltc (mov (indd "R0" "0") "T_CLOSURE"))
        (ltc (mov (indd "R0" "1") "R2"))
        (ltc (mov (indd "R0" "2") (lab body-leb)))
        (ltc (jmp exit-clos-leb))
        
        (labtc body-leb)
        (ltc (push "FP"))
        (ltc (mov "FP" "SP"))
        ;Stack fix
        (let ((lab-exact-args (lab-construct "VAR_EXACT_ARGS_"))
              (lab-exit-stack-fix (lab-construct "EXIT_STACK_FIX_VAR_"))
              (list-length 0))
          (begin (ltc (cmp (fparg "1") (ns list-length)))
                 (ltc (jmp-eq lab-exact-args))
                 (ltc (push "R1"))
                 (ltc (push "R2"))
                 (ltc (push "R3"))
                 (ltc (call "MAKE_SOB_NIL"))
                 (ltc (mov "R1" "R0"))
                 (ltc (mov "R2" (fparg "1")))
                 (ltc (sub "R2" (ns list-length)))
                 (ltc (mov "R3" (fparg "1")))
                 (ltc (add "R3" "1"))
                 (for "R2"
                   (lambda ()
                     (ltc (push "R1"))
                     (ltc (push (fparg "R3")))
                     (ltc (call "MAKE_SOB_PAIR"))
                     (ltc (drop "2"))
                     (ltc (mov "R1" "R0"))
                     (ltc (decr "R3"))))
                 (ltc (mov (fparg (ns (+ 2 list-length))) "R1"))
                 (ltc (pop "R3"))
                 (ltc (pop "R2"))
                 (ltc (pop "R1"))
                 (ltc (jmp lab-exit-stack-fix))

                 (labtc lab-exact-args)
                 (ltc (push "R2"))
                 (ltc (push "R3"))
                 (ltc (push "R4"))
                 (ltc (mov "R2" (ns (+ 4 list-length))))
                 (ltc (mov "R3" (ns -2)))
                 (for "R2"
                   (lambda () (begin (ltc (mov "R4" (fparg "R3")))
                                     (ltc (decr "R3"))
                                     (ltc (mov (fparg "R3") "R4"))
                                     (ltc (incr "R3"))
                                     (ltc (incr "R3"))
                                     )))
                 (ltc (incr "FP"))
                 (ltc (incr "SP"))
                 (ltc (mov "R2" (fparg "1")))
                 (ltc (incr "R2"))
                 (ltc (mov (fparg "1") "R2"))
                 (ltc (call "MAKE_SOB_NIL"))
                 (ltc (mov (fparg (ns (+ 2 list-length))) "R0"))
                 (ltc (pop "R4"))
                 (ltc (pop "R3"))
                 (ltc (pop "R2"))
                 (labtc lab-exit-stack-fix)
                 (set! major (+ 1  major))
                 (code-gen (caddr pe))
                 (set! major (- 1 major))
                 (ltc (pop "FP"))
                 (ltc "RETURN")
                 (labtc exit-clos-leb)))
        ))))

(define gen-applic-tc
  (lambda (pe)
    (let ((lab-sp-fixed (lab-construct "SP_FIXED_")))
    (begin (ltc (cmp (fparg "1") (ns (length (caddr pe)))))
           (ltc (jmp-le lab-sp-fixed))         
           (labtc lab-sp-fixed)
           
           (map-in-order
            (lambda (l)
              (begin (code-gen l)
                     (ltc (push "R0"))))
            (reverse (caddr pe)))
           (ltc (push (imm (ns (length (caddr pe))))))
           (code-gen (cadr pe))
           (ltc (cmp (indd "R0" "0") "T_CLOSURE"))
           (ltc (jmp-ne "NOT_CLOSURE"))
           (ltc (sa (push (indd "R0" "1")) "/*env*/"))
           (ltc (push (fparg "-1")))
           (ltc (mov "R1" (fparg "-2")))
           (ltc (mov "R2" (ns (length (caddr pe)))))
           (ltc (add "R2" "3"))
           (ltc (mov "R3" "SP"))
           (ltc (sub "R3" "3")) ;pushed env num of args and one
           (ltc (sub "R3" (ns (length (caddr pe)))))
           (ltc (mov "R4" "FP"))
           (ltc (sub "R4" "4"))
           (ltc (sub "R4" (fparg "1")))
           (for "R2"
             (lambda () (begin (ltc (mov "R5" (stack "R3")))                               
                               (ltc (mov (stack "R4") "R5"))
                               (ltc (incr "R3"))
                               (ltc (incr "R4")))))
           (ltc (mov "FP" "R1"))
           (ltc (mov "SP" "R4"))
           (ltc (jmp (sa "*" (indd "R0" "2"))))
           ))))


(define gen-pvar
  (lambda (pe)
    (ltc (mov "R0" (fparg (ns (+ 2 (caddr pe))))))
    ))

(define gen-bvar
  (lambda (pe)
    (let ((major (ns (caddr pe)))
          (minor (ns (cadddr pe))))
      (ltc (mov "R0" (fparg "0")))
      (ltc (mov "R0" (indd "R0" major)))
      (ltc (mov "R0" (indd "R0" minor)))
      )))

    
            

(define gen-def
  (lambda (pe)
    (cond ((equal? 'fvar (caadr pe)) (def-fvar pe))
          (else "NOT IMPL"))
    ))

(define set-pvar
  (lambda (pe)
    (begin (code-gen (caddr pe))
           (ltc (mov (fparg (ns (+ 2 (caddr (cadr pe))))) "R0"))
           (ltc (mov "R0" "T_VOID"))
           )))

(define set-bvar
  (lambda (pe)
    (let ((major (caddr (cadr pe)))
          (minor (cadddr (cadr pe))))
      (code-gen (caddr pe))
      (ltc (mov "R1" (fparg "0")))
      (ltc (mov "R1" (indd "R1" (ns major))))
      (ltc (mov (indd "R1" (ns minor)) "R0")) 
      (ltc (mov "R0" "T_VOID"))
      )))

(define gen-set
  (lambda (pe)
    (cond ((equal? 'fvar (caadr pe)) (set-fvar pe))
          ((equal? 'pvar (caadr pe)) (set-pvar pe))
          ((equal? 'bvar (caadr pe)) (set-bvar pe))
          (else "NOT IMPL")) ;TODO
    ))
  
(define gen-box-get-pvar
  (lambda (pe)
    (begin (ltc (mov "R0" (fparg (ns (+ 2 (caddr (cadr pe)))))))
           (ltc (mov "R0" (ind "R0")))
           )))

(define gen-box-get-bvar
  (lambda (pe)
    (let ((major (ns (caddr (cadr pe))))
          (minor (ns (cadddr (cadr pe)))))
      (ltc (mov "R0" (fparg "0")))
      (ltc (mov "R0" (indd "R0" major)))
      (ltc (mov "R0" (indd "R0" minor)))
      (ltc (mov "R0" (ind "R0")))
      )))

(define gen-box-get
  (lambda (pe)
    (cond ((equal? 'fvar (caadr pe)) (gen-box-get-fvar pe))
          ((equal? 'pvar (caadr pe)) (gen-box-get-pvar pe))
          ((equal? 'bvar (caadr pe)) (gen-box-get-bvar pe))
          (else "NOT IMPL")) ;TODO
    ))

(define gen-box-set-fvar
  (lambda (pe)
    (code-gen (caddr pe))
    (ltc (mov "R1" (lookup-global (cadr pe))))
    (ltc (add "R1" "R15"))
    (ltc (mov "R1" (ind "R1")))
    (ltc (mov (ind "R1") "R0"))
    (ltc (mov "R0" "T_VOID"))
    ))

(define gen-box-set-pvar
  (lambda (pe)
    (code-gen (caddr pe))
    (ltc (mov "R1" (fparg (ns (+ 2 (caddr (cadr pe)))))))
    (ltc (mov (ind "R1") "R0"))
    (ltc (mov "R0" "T_VOID"))
    ))

(define gen-box-set-bvar
  (lambda (pe)
    (let ((major (ns (caddr (cadr pe))))
          (minor (ns (cadddr (cadr pe)))))
      (code-gen (caddr pe))
      (ltc (mov "R1" (fparg "0")))
      (ltc (mov "R1" (indd "R1" major)))
      (ltc (mov "R1" (indd "R1" minor)))
      (ltc (mov (ind "R1") "R0"))
      )))

(define gen-box-set
  (lambda (pe)
    (cond ((equal? 'fvar (caadr pe)) (gen-box-set-fvar pe))
          ((equal? 'pvar (caadr pe)) (gen-box-set-pvar pe))
          ((equal? 'bvar (caadr pe)) (gen-box-set-bvar pe))
          (else "NOT IMPL")) ;TODO
    ))

(define gen-const
  (lambda (pe)
    (ltc (mov "R0" (imm (ns (const-lookup (cadr pe) const-table)))))
    ))

(define box-pvar
  (lambda (pe)
    (let ((minor (caddr (cadr pe))))
      (ltc (push "1"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") (fparg (ns (+ 2 minor)))))
      )))

(define box-bvar 
  (lambda (pe)
    (let ((major (ns (caddr (cadr pe))))
          (minor (ns (cadddr (cadr pe)))))
      (ltc (push "1"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov "R1" (fparg "0")))
      (ltc (mov "R1" (indd "R1" major)))
      (ltc (mov "R1" (indd "R1" minor)))
      (ltc (mov (ind "R0") "R1"))
      )))

(define box-fvar 
  (lambda (pe)
    (ltc (mov "R1" (lookup-global (cadr pe))))
    (ltc (add "R1" "R15"))
    (ltc (push "1"))
    (ltc (call "MALLOC"))
    (ltc (drop "1"))
    (ltc (mov (ind "R0") "R1"))
    ))

(define gen-box
  (lambda (pe)
    (cond ((equal? 'fvar (caadr pe)) (box-fvar pe))
          ((equal? 'pvar (caadr pe)) (box-pvar pe))
          ((equal? 'bvar (caadr pe)) (box-bvar pe))
          (else "NOT IMPL")) ;TODO
    ))
    

(define code-gen
  (lambda (pe)
    (cond ((or (not (pair? pe)) (null? pe)) "")
          ((equal? 'fvar (car pe)) (gen-fvar pe))
          ((equal? 'def (car pe)) (gen-def pe)) ;TODO
          ((equal? 'set (car pe)) (gen-set pe)) ;TODO
          ((equal? 'seq (car pe)) (map-in-order code-gen (cadr pe)))
          ((equal? 'if3 (car pe)) (gen-if3 pe))
          ((equal? 'or (car pe)) (gen-or pe))
          ((equal? 'applic (car pe)) (gen-applic pe))
          ((equal? 'lambda-simple (car pe)) (gen-lambda-simple pe))
          ((equal? 'lambda-opt (car pe)) (gen-lambda-opt pe))   
          ((equal? 'lambda-var (car pe)) (gen-lambda-var pe))
          ((equal? 'box-get (car pe)) (gen-box-get pe))
          ((equal? 'box (car pe)) (gen-box pe))
          ((equal? 'box-set (car pe)) (gen-box-set pe))
          ((equal? 'pvar (car pe)) (gen-pvar pe))
          ((equal? 'bvar (car pe)) (gen-bvar pe))
          ((equal? 'tc-applic (car pe)) (gen-applic-tc pe))
          ((equal? 'const (car pe)) (gen-const pe))        
          (else (begin (code-gen (car pe)) (code-gen (cdr pe)))))
    ))


;---------------------------------------------------------------------------------
;--------------------------------------RUN TIME-----------------------------------
;---------------------------------------------------------------------------------

(define make-cons
  (lambda ()
    (let ((body-lab "LconsBody")
          (closure-lab "LmakeConsClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "2"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (push (fparg "3")))
      (ltc (push (fparg "2")))
      (ltc (call "MAKE_SOB_PAIR"))
      (ltc (drop "2"))
      (ltc (pop "FP"))
      (ltc "RETURN")

      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "8888"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'cons)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-car
  (lambda ()
    (let ((body-lab "LcarBody")
          (closure-lab "LmakeCarClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "1"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R1" (fparg "2")))
      (ltc (cmp (ind "R1") "T_PAIR"))
      (ltc (jmp-ne "ERROR"))
      (ltc (mov "R0" (indd "R1" "1")))
      (ltc (pop "FP"))
      (ltc "RETURN")

      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "99999"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'car)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-cdr
  (lambda ()
    (let ((body-lab "LcdrBody")
          (closure-lab "LmakeCdrClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "1"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R1" (fparg "2")))
      (ltc (cmp (ind "R1") "T_PAIR"))
      (ltc (jmp-ne "ERROR"))
      (ltc (mov "R0" (indd "R1" "2")))
      (ltc (pop "FP"))
      (ltc "RETURN")

      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "454545"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'cdr)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-denominator
  (lambda ()
    (let ((body-lab "LDenBody")
          (closure-lab "LmakeDenClos")
          (maybe-int "MayBeIntDenom")
          (exit-deno "ExitDenomi"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "1"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R0" (fparg "2")))
      (ltc (cmp (ind "R0") "T_FRACTION"))
      (ltc (jmp-ne maybe-int))
      (ltc (push (indd "R0" "2")))
      (ltc (jmp exit-deno))
      (labtc maybe-int)
      (ltc (cmp (ind "R0") "T_INTEGER"))
      (ltc (jmp-ne "ERROR"))
      (ltc (push (imm "1")))
      (labtc exit-deno)
      (ltc (call "MAKE_SOB_INTEGER"))
      (ltc (drop "1"))
      (ltc (pop "FP"))
      (ltc "RETURN")

      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "454545"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'denominator)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-numerator
  (lambda ()
    (let ((body-lab "LNumBody")
          (closure-lab "LmakeNumClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "1"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R0" (fparg "2")))
      (ltc (push (indd "R0" "1")))
      (ltc (call "MAKE_SOB_INTEGER"))
      (ltc (drop "1"))
      (ltc (pop "FP"))
      (ltc "RETURN")

      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "454545"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'numerator)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-set-car
  (lambda ()
    (let ((body-lab "LSetcarBody")
          (closure-lab "LmakeSetCarClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "2"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R1" (fparg "2")))
      (ltc (cmp (ind "R1") "T_PAIR"))
      (ltc (jmp-ne "ERROR"))
      (ltc (mov (indd "R1" "1") (fparg "3")))
      (ltc (mov "R0" "T_VOID"))
      (ltc (pop "FP"))
      (ltc "RETURN")

      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "121212"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'set-car!)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-set-cdr
  (lambda ()
    (let ((body-lab "LSetcdrBody")
          (closure-lab "LmakeSetCdrClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "2"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R1" (fparg "2")))
      (ltc (cmp (ind "R1") "T_PAIR"))
      (ltc (jmp-ne "ERROR"))
      (ltc (mov (indd "R1" "2") (fparg "3")))
      (ltc (mov "R0" "T_VOID"))
      (ltc (pop "FP"))
      (ltc "RETURN")

      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "1313131"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'set-cdr!)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

;------------------------------------predicate-----------------------------------------

(define predicate-maker
  (lambda (proc-name type closure-lab body-lab)
    (ltc (jmp closure-lab))
    (labtc body-lab)
    (ltc (push "FP"))
    (ltc (mov "FP" "SP"))
    (ltc (cmp (fparg "1") "1"))
    (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
    (ltc (mov "R1" (fparg "2")))
    (ltc (cmp (ind "R1") type))
    (let ((not-type (lab-construct "NOT_TYPE_"))
          (exit-type (lab-construct "EXIT_TYPE_")))
      (ltc (jmp-ne not-type))
      (ltc (mov "R0" (ns (const-lookup #t const-table))))
      (ltc (jmp exit-type))
      (labtc not-type)
      (ltc (mov "R0" (ns (const-lookup #f const-table))))
      (labtc exit-type)
      (ltc (pop "FP"))
      (ltc "RETURN"))

    (labtc closure-lab)
    (ltc (push "3"))
    (ltc (call "MALLOC"))
    (ltc (drop "1"))
    (ltc (mov (ind "R0") "T_CLOSURE"))
    (ltc (mov (indd "R0" "1") "1515"))
    (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
    (ltc (mov "R1" (lookup-global proc-name)))
    (ltc (add "R1" "R15"))
    (ltc (mov (ind "R1") "R0"))
    ))

(define make-pair?
  (lambda ()
    (predicate-maker 'pair? "T_PAIR" (lab-construct "TYPE_CLOS_") (lab-construct "TYPE_BODY_"))
    ))

(define make-boolean?
  (lambda ()
    (predicate-maker 'boolean? "T_BOOL" (lab-construct "TYPE_CLOS_") (lab-construct "TYPE_BODY_"))
    ))

(define make-char?
  (lambda ()
    (predicate-maker 'char? "T_CHAR" (lab-construct "TYPE_CLOS_") (lab-construct "TYPE_BODY_"))
    ))

(define make-null?
  (lambda ()
    (predicate-maker 'null? "T_NIL" (lab-construct "TYPE_CLOS_") (lab-construct "TYPE_BODY_"))
    ))

(define make-procedure?
  (lambda ()
    (predicate-maker 'procedure? "T_CLOSURE" (lab-construct "TYPE_CLOS_") (lab-construct "TYPE_BODY_"))
    ))
(define make-string?
  (lambda ()
    (predicate-maker 'string? "T_STRING" (lab-construct "TYPE_CLOS_") (lab-construct "TYPE_BODY_"))
    ))
(define make-symbol?
  (lambda ()
    (predicate-maker 'symbol? "T_SYMBOL" (lab-construct "TYPE_CLOS_") (lab-construct "TYPE_BODY_"))
    ))

(define make-vector?
  (lambda ()
    (predicate-maker 'vector? "T_VECTOR" (lab-construct "TYPE_CLOS_") (lab-construct "TYPE_BODY_"))
    ))

(define make-integer?
  (lambda ()
    (let ((closure-lab (lab-construct "TYPE_CLOS_"))
          (body-lab (lab-construct "TYPE_BODY_")))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "1"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R1" (fparg "2")))
      (ltc (cmp (ind "R1") "T_INTEGER"))
      (let ((not-type (lab-construct "NOT_TYPE_"))
            (not-int (lab-construct "NOT_INT"))
            (exit-type (lab-construct "EXIT_TYPE_")))
        (ltc (jmp-ne not-int))
        (ltc (mov "R0" (ns (const-lookup #t const-table))))
        (ltc (jmp exit-type))
        (labtc not-int)
        (ltc (cmp (ind "R1") "T_FRACTION"))
        (ltc (jmp-ne not-type))
        (ltc (cmp (indd "R1" "2") (imm "1")))
        (ltc (jmp-ne not-type))
        (ltc (mov "R0" (ns (const-lookup #t const-table))))
        (ltc (jmp exit-type))
        (labtc not-type)
        (ltc (mov "R0" (ns (const-lookup #f const-table))))
        (labtc exit-type)
        (ltc (pop "FP"))
        (ltc "RETURN"))
      
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "1515"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'integer?)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-zero?
  (lambda ()
    (let ((proc-name 'zero?)
          (closure-lab (lab-construct "TYPE_CLOS_"))
          (body-lab (lab-construct "TYPE_BODY_")))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "1"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R1" (fparg "2")))
      (ltc (cmp (indd "R1" "1") (imm "0")))
      (let ((not-type (lab-construct "NOT_TYPE_"))
            (exit-type (lab-construct "EXIT_TYPE_")))
        (ltc (jmp-ne not-type))
        (ltc (mov "R0" (ns (const-lookup #t const-table))))
        (ltc (jmp exit-type))
        (labtc not-type)
        (ltc (mov "R0" (ns (const-lookup #f const-table))))
        (labtc exit-type)
        (ltc (pop "FP"))
        (ltc "RETURN"))
      
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "1515"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global proc-name)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

 

(define maker-number?-rational?
  (lambda (proc-name)
    (let ((closure-lab (lab-construct "TYPE_CLOS_"))
          (body-lab (lab-construct "TYPE_BODY_")))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "1"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R1" (fparg "2")))
      (ltc (cmp (ind "R1") "T_INTEGER"))
      (let ((not-type1 (lab-construct "NOT_TYPE1_"))
            (exit-type (lab-construct "EXIT_TYPE_")))
        (ltc (jmp-ne not-type1))
        (ltc (mov "R0" (ns (const-lookup #t const-table))))
        (ltc (jmp exit-type))
        (labtc not-type1)
        (ltc (cmp (ind "R1") "T_FRACTION"))
        (let ((not-type2 (lab-construct "NOT_TYPE2_")))
          (ltc (jmp-ne not-type2))
          (ltc (mov "R0" (ns (const-lookup #t const-table))))
          (ltc (jmp exit-type))
          (labtc not-type2)
          (ltc (mov "R0" (ns (const-lookup #f const-table))))
          (labtc exit-type)
          (ltc (pop "FP"))
          (ltc "RETURN")))
      
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "1515"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global proc-name)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-rational? (lambda () (maker-number?-rational? 'rational?)))
(define make-number? (lambda () (maker-number?-rational? 'number?)))
;---------------------------------end of predicate---------------------------------------------

(define make-char->integer
  (lambda ()
    (let ((body-lab "LCharToIntBody")
          (closure-lab "LmakeCharToIntClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "1"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R0" (fparg "2")))
      (ltc (cmp (ind "R0") "T_CHAR"))
      (ltc (jmp-ne "ERROR"))
      (ltc (mov (indd "R0" "0") "T_INTEGER"))
      (ltc (pop "FP"))
      (ltc "RETURN")

      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "963963"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'char->integer)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-integer->char
  (lambda ()
    (let ((body-lab "LIntToCharBody")
          (closure-lab "LmakeIntToCharClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (cmp (fparg "1") "1"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R0" (fparg "2")))
      (ltc (cmp (ind "R0") "T_INTEGER"))
      (ltc (jmp-ne "ERROR"))
      (ltc (mov (indd "R0" "0") "T_CHAR"))
      (ltc (pop "FP"))
      (ltc "RETURN")

      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "963963"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'integer->char)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-not
  (lambda ()
    (let ((body-lab "LNotBody")
          (closure-lab "LmakeNotClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))     
      (ltc (cmp (fparg "1") "1"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R0" (ns (const-lookup #f const-table))))
      (ltc (cmp (fparg "2") "R0"))
      (let ((exit-lab (lab-construct "EXIT_NOT_")))
        (ltc (jmp-ne exit-lab))
        (ltc (mov "R0" (ns (const-lookup #t const-table))))
        (labtc exit-lab)
        (ltc (pop "FP"))
        (ltc "RETURN"))
      
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "8546845"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'not)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-len-str-or-vec
  (lambda (proc-name type body-lab closure-lab)
    (ltc (jmp closure-lab))
    (labtc body-lab)
    (ltc (push "FP"))
    (ltc (mov "FP" "SP"))
    (ltc (cmp (fparg "1") "1"))
    (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
    (ltc (mov "R1" (fparg "2")))
    (ltc (cmp (ind "R1") type))
    (ltc (jmp-ne "ERROR"))
    (ltc (push (indd "R1" "1")))
    (ltc (call "MAKE_SOB_INTEGER"))
    (ltc (drop "1"))
    (ltc (pop "FP"))
    (ltc "RETURN")
    
    (labtc closure-lab)
    (ltc (push "3"))
    (ltc (call "MALLOC"))
    (ltc (drop "1"))
    (ltc (mov (ind "R0") "T_CLOSURE"))
    (ltc (mov (indd "R0" "1") "851479"))
    (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
    (ltc (mov "R1" (lookup-global proc-name)))
    (ltc (add "R1" "R15"))
    (ltc (mov (ind "R1") "R0"))
    ))

(define make-len-str (lambda () (make-len-str-or-vec 'string-length "T_STRING" 
                                          (lab-construct "STR_LEN_BOD_") (lab-construct "STR_LEN_CLO_"))))

(define make-len-vec (lambda () (make-len-str-or-vec 'vector-length "T_VECTOR" 
                                          (lab-construct "VEC_LEN_BOD_") (lab-construct "VEC_LEN_CLO_"))))

(define make-str-ref
  (lambda ()
     (let ((body-lab "StrRefBody")
          (closure-lab "StrRefClos"))
       (ltc (jmp closure-lab))
       (labtc body-lab)
       (ltc (push "FP"))
       (ltc (mov "FP" "SP"))     
       (ltc (cmp (fparg "1") "2"))
       (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
       (ltc (mov "R2" (indd (fparg "3") "1")))
       (ltc (add "R2" "2"))
       (ltc (mov "R1" (fparg "2")))
       (ltc (mov "R0" (indd "R1" "R2")))
       (ltc (push "R0"))
       (ltc (call "MAKE_SOB_CHAR"))
       (ltc (drop "1"))
       (ltc (pop "FP"))
       (ltc "RETURN")
       
       (labtc closure-lab)
       (ltc (push "3"))
       (ltc (call "MALLOC"))
       (ltc (drop "1"))
       (ltc (mov (ind "R0") "T_CLOSURE"))
       (ltc (mov (indd "R0" "1") "8546845"))
       (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
       (ltc (mov "R1" (lookup-global 'string-ref)))
       (ltc (add "R1" "R15"))
       (ltc (mov (ind "R1") "R0"))
       )))

(define make-vec-ref
  (lambda ()
    (let ((body-lab "VecRefBody")
          (closure-lab "VecRefClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))     
      (ltc (cmp (fparg "1") "2"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R2" (indd (fparg "3") "1")))
      (ltc (add "R2" "2"))
      (ltc (mov "R1" (fparg "2")))
      (ltc (mov "R0" (indd "R1" "R2")))
      (ltc (pop "FP"))
      (ltc "RETURN")
      
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "8546845"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'vector-ref)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-vec-set
  (lambda ()
    (let ((body-lab "VecSetBody")
          (closure-lab "VecSetClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))     
      (ltc (cmp (fparg "1") "3"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R0" (indd (fparg "3") "1")))
      (ltc (add "R0" "2"))
      (ltc (mov "R1" (fparg "2")))
      (ltc (mov (indd "R1" "R0") (fparg "4")))
      (ltc (mov "R0" "T_VOID"))
      (ltc (pop "FP"))
      (ltc "RETURN")
      
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "8546845"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'vector-set!)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-str-set
  (lambda ()
    (let ((body-lab "StrSetBody")
          (closure-lab "StrSetClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))     
      (ltc (cmp (fparg "1") "3"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R0" (indd (fparg "3") "1")))
      (ltc (add "R0" "2"))
      (ltc (mov "R1" (fparg "2")))
      (ltc (mov (indd "R1" "R0") (indd (fparg "4") "1")))
      (ltc (mov "R0" "T_VOID"))
      (ltc (pop "FP"))
      (ltc "RETURN")
      
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "8546845"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'string-set!)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-remainder
  (lambda ()
    (let ((body-lab "RemainderBody")
          (closure-lab "RemainderClos"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))     
      (ltc (cmp (fparg "1") "2"))
      (ltc (jmp-ne "ERROR_NUM_OF_ARG"))
      (ltc (mov "R0" (indd (fparg "2") "1")))
      (ltc (mov "R1" (indd (fparg "3") "1")))
      (ltc (rem "R0" "R1"))
      (ltc (push "R0"))
      (ltc (call "MAKE_SOB_INTEGER"))
      (ltc (drop "1"))
      (ltc (pop "FP"))
      (ltc "RETURN")
      
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "8546845"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'remainder)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define for2
  (lambda (i do)
    (let ((exit-loop (lab-construct "FOR_EXIT_"))
          (start-loop (lab-construct "FOR_START_")))
      (ltc (mov "R10" i))
      (labtc start-loop)
      (ltc (cmp "R10" "0"))
      (ltc (jmp-eq exit-loop))
      (do)
      (ltc (decr "R10"))
      (ltc (jmp start-loop))
      (labtc exit-loop)  
    )))

(define make-vector-vector-runtime
  (lambda (proc-name type body-lab closure-lab)
    (ltc (jmp closure-lab))
    (labtc body-lab)
    (ltc (push "FP"))
    (ltc (mov "FP" "SP"))     
    (ltc (mov "R1" (fparg "1")))
    (ltc (mov "R2" "2"))
    (for2 "R1" (lambda () (begin (ltc (mov "R3" (fparg "R2")))
                                  (ltc (push "R3"))
                                  (ltc (incr "R2")))))
    (ltc (mov "R3" (fparg "1")))
    (ltc (push "R3"))
    (ltc (call type))
    (ltc (incr "R3"))
    (ltc (drop "R3"))
    (ltc (pop "FP"))
    (ltc "RETURN")
    
    (labtc closure-lab)
    (ltc (push "3"))
    (ltc (call "MALLOC"))
    (ltc (drop "1"))
    (ltc (mov (ind "R0") "T_CLOSURE"))
    (ltc (mov (indd "R0" "1") "8546845"))
    (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
    (ltc (mov "R1" (lookup-global proc-name)))
    (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
    ))

(define make-vector-runtime (lambda () (make-vector-vector-runtime 'vector "MAKE_SOB_VECTOR" "VecRunBod" "VecRunClo")))

(define make-string-string-runtime
  (lambda (proc-name type body-lab closure-lab)
    (ltc (jmp closure-lab))
    (labtc body-lab)
    (ltc (push "FP"))
    (ltc (mov "FP" "SP"))     
    (ltc (mov "R1" (fparg "1")))
    (ltc (mov "R2" "2"))
    (for2 "R1" (lambda () (begin (ltc (mov "R3" (indd (fparg "R2") "1")))
                                  (ltc (push "R3"))
                                  (ltc (incr "R2")))))
    (ltc (mov "R3" (fparg "1")))
    (ltc (push "R3"))
    (ltc (call type))
    (ltc (incr "R3"))
    (ltc (drop "R3"))
    (ltc (pop "FP"))
    (ltc "RETURN")
    
    (labtc closure-lab)
    (ltc (push "3"))
    (ltc (call "MALLOC"))
    (ltc (drop "1"))
    (ltc (mov (ind "R0") "T_CLOSURE"))
    (ltc (mov (indd "R0" "1") "8546845"))
    (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
    (ltc (mov "R1" (lookup-global proc-name)))
    (ltc (add "R1" "R15"))
    (ltc (mov (ind "R1") "R0"))
    ))

(define make-string-runtime (lambda () (make-string-string-runtime 'string "MAKE_SOB_STRING" "StrRunBod" "StrRunClo")))

(define make-make-string-runtime-help
  (lambda (proc-name type body-lab closure-lab)
    (ltc (jmp closure-lab))
    (labtc body-lab)
    (ltc (push "FP"))
    (ltc (mov "FP" "SP"))     
    (ltc (mov "R1" (indd (fparg "2") "1")))
    (ltc (mov "R2" (indd (fparg "3") "1")))
    (for2 "R1" (lambda () (ltc (push "R2"))))
    (ltc (mov "R3" (indd (fparg "2") "1")))
    (ltc (push "R3"))
    (ltc (call type))
    (ltc (incr "R3"))
    (ltc (drop "R3"))
    (ltc (pop "FP"))
    (ltc "RETURN")
    
    (labtc closure-lab)
    (ltc (push "3"))
    (ltc (call "MALLOC"))
    (ltc (drop "1"))
    (ltc (mov (ind "R0") "T_CLOSURE"))
    (ltc (mov (indd "R0" "1") "8546845"))
    (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
    (ltc (mov "R1" (lookup-global proc-name)))
    (ltc (add "R1" "R15"))
    (ltc (mov (ind "R1") "R0"))
    ))

(define make-make-str (lambda () (make-make-string-runtime-help 'make-string "MAKE_SOB_STRING" (lab-construct "STR_MAKE_BOD_")
                                                                (lab-construct "STR_MAKE_CLO_"))))

(define make-make-vector-runtime-help
  (lambda (proc-name type body-lab closure-lab)
    (ltc (jmp closure-lab))
    (labtc body-lab)
    (ltc (push "FP"))
    (ltc (mov "FP" "SP"))     
    (ltc (mov "R1" (indd (fparg "2") "1")))
    (ltc (mov "R2" (fparg "3")))
    (for2 "R1" (lambda () (ltc (push "R2"))))
    (ltc (mov "R3" (indd (fparg "2") "1")))
    (ltc (push "R3"))
    (ltc (call type))
    (ltc (incr "R3"))
    (ltc (drop "R3"))
    (ltc (pop "FP"))
    (ltc "RETURN")
    
    (labtc closure-lab)
    (ltc (push "3"))
    (ltc (call "MALLOC"))
    (ltc (drop "1"))
    (ltc (mov (ind "R0") "T_CLOSURE"))
    (ltc (mov (indd "R0" "1") "8546845"))
    (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
    (ltc (mov "R1" (lookup-global proc-name)))
    (ltc (add "R1" "R15"))
    (ltc (mov (ind "R1") "R0"))
    ))

(define make-make-vec (lambda () (make-make-vector-runtime-help 'make-vector "MAKE_SOB_VECTOR" (lab-construct "VEC_MAKE_BOD_")
                                                       (lab-construct "VEC_MAKE_CLO_"))))

(define make-plus-two
  (lambda ()
    (let ((body-lab "Plus_BODY")
          (closure-lab "Plus_CLOSURE")
          (first-not-int "FIRST_NOT_INT")
          (check-second-int "CHECK_SECOND_INT")
          (second-not-int "SECONS_NOT_INT")
          (exit-check-int "EXIT_INT_CHECK"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (mov "R1" (indd (fparg "2") "1")))
      (ltc (mov "R3" (indd (fparg "3") "1")))
      (ltc (cmp (ind (fparg "2")) "T_INTEGER"))
      (ltc (jmp-ne first-not-int))
      (ltc (mov "R2" "1"))
      (ltc (jmp check-second-int))
      (labtc first-not-int)
      (ltc (mov "R2" (indd (fparg "2") "2")))
      (labtc check-second-int)
      (ltc (cmp (ind (fparg "3")) "T_INTEGER"))
      (ltc (jmp-ne second-not-int))
      (ltc (mov "R4" "1"))
      (ltc (jmp exit-check-int))
      (labtc second-not-int)
      (ltc (mov "R4" (indd (fparg "3") "2")))
      (labtc exit-check-int)
      (ltc (mul "R1" "R4"))
      (ltc (mul "R3" "R2"))
      (ltc (add "R1" "R3"))
      (ltc (mul "R2" "R4"))
      (ltc (push "R1"))
      (ltc (push "R2"))
      (ltc (call "MAKE_SOB_FRACTION"))
      (ltc (drop "2"))
      (ltc (pop "FP"))
      (ltc "RETURN")
    
    
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "852963"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'plus-two)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-minus-two
  (lambda ()
    (let ((body-lab "Minus_BODY")
          (closure-lab "Minus_CLOSURE")
          (first-not-int "Minus_FIRST_NOT_INT")
          (check-second-int "Minus_CHECK_SECOND_INT")
          (second-not-int "Minus_SECONS_NOT_INT")
          (exit-check-int "Minus_EXIT_INT_CHECK"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (mov "R1" (indd (fparg "2") "1")))
      (ltc (mov "R3" (indd (fparg "3") "1")))
      (ltc (cmp (ind (fparg "2")) "T_INTEGER"))
      (ltc (jmp-ne first-not-int))
      (ltc (mov "R2" "1"))
      (ltc (jmp check-second-int))
      (labtc first-not-int)
      (ltc (mov "R2" (indd (fparg "2") "2")))
      (labtc check-second-int)
      (ltc (cmp (ind (fparg "3")) "T_INTEGER"))
      (ltc (jmp-ne second-not-int))
      (ltc (mov "R4" "1"))
      (ltc (jmp exit-check-int))
      (labtc second-not-int)
      (ltc (mov "R4" (indd (fparg "3") "2")))
      (labtc exit-check-int)
      (ltc (mul "R1" "R4"))
      (ltc (mul "R3" "R2"))
      (ltc (sub "R1" "R3"))
      (ltc (mul "R2" "R4"))
      (ltc (push "R1"))
      (ltc (push "R2"))
      (ltc (call "MAKE_SOB_FRACTION"))
      (ltc (drop "2"))
      (ltc (pop "FP"))
      (ltc "RETURN")
    
    
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "852963"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'minus-two)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-mul-two
  (lambda ()
    (let ((body-lab "Mul_BODY")
          (closure-lab "MUL_CLOSURE")
          (first-not-int "Mul_FIRST_NOT_INT")
          (check-second-int "Mul_CHECK_SECOND_INT")
          (second-not-int "Mul_SECONS_NOT_INT")
          (exit-check-int "Mul_EXIT_INT_CHECK"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (mov "R1" (indd (fparg "2") "1")))
      (ltc (mov "R3" (indd (fparg "3") "1")))
      (ltc (cmp (ind (fparg "2")) "T_INTEGER"))
      (ltc (jmp-ne first-not-int))
      (ltc (mov "R2" "1"))
      (ltc (jmp check-second-int))
      (labtc first-not-int)
      (ltc (mov "R2" (indd (fparg "2") "2")))
      (labtc check-second-int)
      (ltc (cmp (ind (fparg "3")) "T_INTEGER"))
      (ltc (jmp-ne second-not-int))
      (ltc (mov "R4" "1"))
      (ltc (jmp exit-check-int))
      (labtc second-not-int)
      (ltc (mov "R4" (indd (fparg "3") "2")))
      (labtc exit-check-int)
      (ltc (mul "R1" "R3"))
      (ltc (mul "R2" "R4"))
      (ltc (push "R1"))
      (ltc (push "R2"))
      (ltc (call "MAKE_SOB_FRACTION"))
      (ltc (drop "2"))
      (ltc (pop "FP"))
      (ltc "RETURN")
    
    
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "852963"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'mul-two)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-div-two
  (lambda ()
    (let ((body-lab "Div_BODY")
          (closure-lab "Div_CLOSURE")
          (first-not-int "Div_FIRST_NOT_INT")
          (check-second-int "Div_CHECK_SECOND_INT")
          (second-not-int "Div_SECONS_NOT_INT")
          (exit-check-int "Div_EXIT_INT_CHECK"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (mov "R1" (indd (fparg "2") "1")))
      (ltc (mov "R3" (indd (fparg "3") "1")))
      (ltc (cmp (ind (fparg "2")) "T_INTEGER"))
      (ltc (jmp-ne first-not-int))
      (ltc (mov "R2" "1"))
      (ltc (jmp check-second-int))
      (labtc first-not-int)
      (ltc (mov "R2" (indd (fparg "2") "2")))
      (labtc check-second-int)
      (ltc (cmp (ind (fparg "3")) "T_INTEGER"))
      (ltc (jmp-ne second-not-int))
      (ltc (mov "R4" "1"))
      (ltc (jmp exit-check-int))
      (labtc second-not-int)
      (ltc (mov "R4" (indd (fparg "3") "2")))
      (labtc exit-check-int)
      (ltc (mul "R1" "R4"))
      (ltc (mul "R2" "R3"))
      (ltc (push "R1"))
      (ltc (push "R2"))
      (ltc (call "MAKE_SOB_FRACTION"))
      (ltc (drop "2"))
      (ltc (pop "FP"))
      (ltc "RETURN")
    
    
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "852963"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'div-two)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-math-eq-two
  (lambda ()
    (let ((body-lab "Math_eq_BODY")
          (closure-lab "Math_eq_CLOSURE")
          (first-not-int "Math_eq_FIRST_NOT_INT")
          (check-second-int "Math_eq_CHECK_SECOND_INT")
          (second-not-int "Math_eq_SECONS_NOT_INT")
          (exit-check-int "Math_eq_EXIT_INT_CHECK")
          (not-eq "Math_eq_NOT_eq")
          (exit-eq "MATH_EQ_EX"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (mov "R1" (indd (fparg "2") "1")))
      (ltc (mov "R3" (indd (fparg "3") "1")))
      (ltc (cmp (ind (fparg "2")) "T_INTEGER"))
      (ltc (jmp-ne first-not-int))
      (ltc (mov "R2" "1"))
      (ltc (jmp check-second-int))
      (labtc first-not-int)
      (ltc (mov "R2" (indd (fparg "2") "2")))
      (labtc check-second-int)
      (ltc (cmp (ind (fparg "3")) "T_INTEGER"))
      (ltc (jmp-ne second-not-int))
      (ltc (mov "R4" "1"))
      (ltc (jmp exit-check-int))
      (labtc second-not-int)
      (ltc (mov "R4" (indd (fparg "3") "2")))
      (labtc exit-check-int)
      (ltc (mul "R1" "R4"))
      (ltc (mul "R3" "R2"))
      (ltc (sub "R1" "R3"))
      (ltc (mul "R2" "R4"))
      (ltc (cmp "R1" (imm "0")))
      (ltc (jmp-ne not-eq))
      (ltc (mov "R0" (ns (const-lookup #t const-table))))
      (ltc (jmp exit-eq))
      (labtc not-eq)
      (ltc (mov "R0" (ns (const-lookup #f const-table))))
      (labtc exit-eq)
      (ltc (pop "FP"))
      (ltc "RETURN")
    
    
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "852963"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'math-eq-two)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))

(define make-math-greater-two
  (lambda ()
    (let ((body-lab "Math_greater_BODY")
          (closure-lab "Math_greater_CLOSURE")
          (first-not-int "Math_greater_FIRST_NOT_INT")
          (check-second-int "Math_greater_CHECK_SECOND_INT")
          (second-not-int "Math_greater_SECONS_NOT_INT")
          (exit-check-int "Math_greater_EXIT_INT_CHECK")
          (not-greater1 "Math_greater_NOT_greater1")
          (not-greater2 "Math_greater_NOT_greater2")
          (greater-false "MATH_GREATER_FALSE")
          (exit-eq "MATH_greater_EX"))
      (ltc (jmp closure-lab))
      (labtc body-lab)
      (ltc (push "FP"))
      (ltc (mov "FP" "SP"))
      (ltc (mov "R1" (indd (fparg "2") "1")))
      (ltc (mov "R3" (indd (fparg "3") "1")))
      (ltc (cmp (ind (fparg "2")) "T_INTEGER"))
      (ltc (jmp-ne first-not-int))
      (ltc (mov "R2" "1"))
      (ltc (jmp check-second-int))
      (labtc first-not-int)
      (ltc (mov "R2" (indd (fparg "2") "2")))
      (labtc check-second-int)
      (ltc (cmp (ind (fparg "3")) "T_INTEGER"))
      (ltc (jmp-ne second-not-int))
      (ltc (mov "R4" "1"))
      (ltc (jmp exit-check-int))
      (labtc second-not-int)
      (ltc (mov "R4" (indd (fparg "3") "2")))
      (labtc exit-check-int)
      (ltc (mul "R1" "R4"))
      (ltc (mul "R3" "R2"))
      (ltc (sub "R1" "R3"))
      (ltc (mul "R2" "R4"))
      (ltc (cmp "R1" (imm "0")))
      (ltc (jmp-le not-greater1))
      (ltc (cmp "R2" (imm "0")))
      (ltc (jmp-le greater-false))
      (ltc (mov "R0" (ns (const-lookup #t const-table))))
      (ltc (jmp exit-eq))
      (labtc not-greater1)
      (ltc (cmp "R2" (imm "0")))
      (ltc (jmp-ge greater-false))
      (ltc (mov "R0" (ns (const-lookup #t const-table))))
      (ltc (jmp exit-eq))
      (labtc greater-false)
      (ltc (mov "R0" (ns (const-lookup #f const-table))))
      (labtc exit-eq)
      (ltc (pop "FP"))
      (ltc "RETURN")
    
    
      (labtc closure-lab)
      (ltc (push "3"))
      (ltc (call "MALLOC"))
      (ltc (drop "1"))
      (ltc (mov (ind "R0") "T_CLOSURE"))
      (ltc (mov (indd "R0" "1") "852963"))
      (ltc (mov (indd "R0" "2") (sa "LABEL(" body-lab ")")))
      (ltc (mov "R1" (lookup-global 'math-greater-two)))
      (ltc (add "R1" "R15"))
      (ltc (mov (ind "R1") "R0"))
      )))
;------------------------------------------------------------------------------
;-----------------------------------------Compile-------------------------------

(define file->string
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
                (lambda ()
                  (let ((ch (read-char in-port)))
                    (if (eof-object? ch)
                        (begin
                          (close-input-port in-port)
                          '())
                        (cons ch (run)))))))
        (list->string
         (run))))))

(define string->file
  (lambda (str out-file)
    (if (file-exists? out-file) (delete-file out-file))
    (let ((out-port (open-output-file out-file))
          (l (string->list str)))
      (letrec ((run
                (lambda (ls)
                    (if (not (null? ls))
                        (begin
                          (write-char (car ls) out-port)
                          (run (cdr ls)))
                        (close-output-port out-port)))))
         (run l)))
    ))
(define string->sexpr
  (lambda (str)
    (<sexpr> (string->list str)
             (lambda (e s)
              (list e (list->string s)))
            (lambda (klum) (display `(ERROR SEXPR))))
    ))

(define make-sexpes
  (lambda (str)
    (letrec ((run (lambda (slst rest)
                     (if (eq? rest "") slst
                         (let ((parsed (string->sexpr rest)))
                           (run (append slst (list (car parsed))) (cadr parsed)))))))
      (run '() str))
    ))

(define parse-manipulate
  (lambda (sexps)
    (annotate-tc
     (pe->lex-pe
      (box-set 
       (remove-applic-lambda-nil
        (eliminate-nested-defines
         (parse sexps))))))
    ))


(define string->sexpr
  (lambda (str)
    (<sexpr> (string->list str)
             (lambda (e s)
              (list e (list->string s)))
            (lambda (klum) (display `(ERROR SEXPR))))
    ))

(define make-sexpes
  (lambda (str)
    (letrec ((run (lambda (slst rest)
                     (if (eq? rest "") slst
                         (let ((parsed (string->sexpr rest)))
                           (run (append slst (list (car parsed))) (cadr parsed)))))))
      (run '() str))
    ))

(define parse-manipulate
  (lambda (sexps)
    ;(annotate-tc TODO
     (pe->lex-pe
      (box-set 
       (remove-applic-lambda-nil
        (eliminate-nested-defines
         (parse sexps)))));)
    ))

(define initial-params
  (lambda ()
    (begin (set! CODE "")
           (set! global-table (list))
           (set! constant-table (list))
           (set! major 0)
           (set! next-free-global 0)
           (set! string-in "")
           )))

(define run-time-func-name (list 'cons 'car 'cdr 'set-car! 'set-cdr! 'pair? 'boolean? 'char? 'integer?
                                  'null? 'number? 'procedure? 'rational? 'string? 'symbol? 'vector? 'zero?
                                  'char->integer 'integer->char 'not 'vector-length 'string-length
                                  'make-vector 'make-string 'list
                                  'string-ref 'vector-ref 'vector-set! 'string-set! 'denominator 'numerator
                                  'rational? 'number? 'remainder 'vector 'string 'plus-two 'minus-two 'mul-two
                                  'div-two 'math-eq-two 'math-greater-two
                                  ))
  
(define add-run-to-list
  (lambda ()
    (add-run-time-to-global run-time-func-name)
    ))

(define run-time-func-impl (list make-cons make-car make-cdr make-set-car make-set-cdr make-pair?
                                 make-boolean? make-char? make-integer? make-null? 
                                 make-procedure? make-string? make-symbol? make-vector? make-zero?
                                 make-char->integer make-integer->char make-not make-len-vec make-len-str
                                 make-make-str make-make-vec
                                 make-str-ref make-vec-ref make-vec-set make-denominator make-numerator
                                 make-str-set make-rational? make-number? make-remainder make-vector-runtime
                                 make-string-runtime make-plus-two make-minus-two make-mul-two make-div-two
                                 make-math-eq-two make-math-greater-two
                                 ))

(define add-run-IMPL-function
  (lambda ()
    (map-in-order (lambda (x) (x)) run-time-func-impl)
    ))

(define gen-phrase-print
  (lambda (x) (begin (code-gen x)
                     (let ((lab-void (lab-construct "VOID_NOT_PRINT_")))
                       (ltc (cmp "R0" "T_VOID"))
                       (ltc (jmp-eq lab-void))
                       (ltc (push "R0"))
                       (ltc (call "WRITE_SOB"))
                       (ltc (drop "1"))
                       (ltc (call "NEWLINE"))
                       (labtc lab-void))
                     )))
      
(define compile-scheme-file
  (lambda (in-file out-file)
    (begin (initial-params)
           (set! string-in (file->string "before.scm"))
           (set! string-in (sa string-in (file->string in-file))) ;V
           (set! sexpes (make-sexpes string-in)) ;V
           (set! manipulated (map-in-order parse-manipulate sexpes)) ;V
           (set! CODE (sa CODE (file->string "prolog.c")))
           (make-const-table manipulated)
           (add-run-to-list)
           (make-global-table manipulated)
           (add-run-IMPL-function)
           (map-in-order gen-phrase-print manipulated)
           (set! CODE (sa CODE (file->string "epilog.c")))
           (string->file CODE out-file) ;V
           manipulated
           )))
