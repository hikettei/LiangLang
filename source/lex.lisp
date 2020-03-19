

(in-package #:liang.compiler)



(cl-lex:define-string-lexer *liang-lexer*
    
  ("\"([^\\\"]|\\.)*?\"" (return (values :string (string-trim "\"" $@))))
  ("\\b[0-9]+\\b" (return (values :number (read-from-string $@))))


  ("###\"([^\\\"]|\\.)*?\"( |\\n)\"([^\\\"]|\\.)*?\"###"
   (return (values :macro-body (string-trim "###" $@))))

  ("(\\)(|\\s){)" (return (values :{{ :{{)))
  
  ("}"    (return (values :} :})))
  ("{"    (return (values :{ :{)))

  (";"    (return (values :END :END)))

  ("\\["    (return (values :[ :[)))
  ("\\]"    (return (values :] :])))

  ("\\("   (return (values :|(| :|(| )))
  ("\\)"   (return (values :|)| :|)| )))


  ("(|\\n)\\+(|\\n)"       (return (values :+ '+)))
  ("(|\\n)\\-(|\\n)"       (return (values :- '-)))
  ("(|\\n)\\*(|\\n)"       (return (values :* '*)))
  ("(|\\n)\\/(|\\n)"       (return (values :/ '/)))

  ("(|\\n)\\=(|\\n)"       (return (values := '=)))
    
  ("," (return (values :COMMA :COMMA)))
  ("\\." (return (values :DOT :DOT)))
  ("@" (return (values :AT-MARK :AT-MARK)))
  
  ("[a-zA-Z||0-9||_||\\!||\\?]+" (return (values :funame (read-from-string $@))))
  ("[\\S]+" (return (values :name (read-from-string $@)))))
 
