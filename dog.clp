(import nrc.fuzzy.*)
(import nrc.fuzzy.jess.FuzzyMain)
(import nrc.fuzzy.jess.*)
(load-package nrc.fuzzy.jess.FuzzyFunctions)



;Following are defglobals for dog's attributes and are all Fuzzy variable.
(defglobal ?*dog_age* = (new FuzzyVariable "Dog Age" 0.0 240.0 "months"))
(defglobal ?*dog_breed* = (new FuzzyVariable "Adult Breed Size" 0.0 40.0 "kgs"))
(defglobal ?*dog_weight* = (new FuzzyVariable "Dog weight" 0.0 70.0 "kgs"))
(defglobal ?*dog_deworm* = (new FuzzyVariable "Dog deworming" 0.0 240.0 "months"))
(defglobal ?*dog_vaccine* = (new FuzzyVariable "Dog vaccination" 0.0 240.0 "months"))



; The non-fuzzy global variables
(defglobal  ?*user* = ""
			?*dog* = ""
    		?*recommend* = ""
    		?*weight* = ""
 			?*next* = "")

;user's enter dog status

(defglobal ?*dog_age_user* = "" 
		   ?*dog_breed_user* = "" 
 		   ?*dog_weight_user* = "" 
    	   ?*dog_deworm_user* = ""	   
    	   ?*dog_vaccine_user* = ""
    )


;template for question and answer of user inputs

(deftemplate question
    (slot text)
    (slot type)
    (slot ident))

(deftemplate answer
    (slot ident)
    (slot text))


(deffunction is-of-type (?answer ?type)
    "validation of input"
    (if (eq ?type l-m-h) then
             (return (or(eq ?answer low)(eq ?answer medium)(eq ?answer high)))
        else (if (eq ?type stage) then
             (return (or(eq ?answer one)(eq ?answer two)(eq ?answer three)))
            else (if (eq ?type number) then
            (return (and (numberp ?answer)(> ?answer 0)))
            else (if (eq ?type breed-size) then
                (return (or(eq ?answer small)(eq ?answer medium)(eq ?answer large)))
            else (return (> (str-length ?answer) 0))
            	)
        	)
    	)
  	)  
)

(deffunction ask-user (?question ?type)
    "Ask a question, and return the answer"
    (bind ?answer "")
    (while (not (is-of-type ?answer ?type)) do
        (printout t ?question " ")
        (if (eq ?type number) then
            (printout t crlf"Really?? Positive Number greater than zero only!!"crlf))
        (if (eq ?type l-m-h) then
            (printout t crlf"VALID OPTIONS: low medium high"crlf))
        (if (eq ?type stage) then
            (printout t crlf"VALID OPTIONS: one two three"crlf))
        (if (eq ?type breed-size) then
            (printout t crlf"VALID OPTIONS: small medium large"crlf))
        (bind ?answer (read)))
    (return ?answer))


(defmodule ask)
(defrule ask::ask-question-by-id
    "Ask a question and assert the answer"
    (declare (auto-focus TRUE))
    (MAIN::question (ident ?id) (text ?text) (type ?type))
    (not (MAIN::answer (ident ?id)))
    ?ask <- (MAIN::ask ?id)
    =>
    (bind ?answer (ask-user ?text ?type))
    (assert (MAIN::answer (ident ?id) (text ?answer)))
    (retract ?ask)
    (return))




;Taking in the user input
(defmodule start)
(defrule init
    =>
    (printout t crlf crlf)
    (printout t "**************************************************************************************************" crlf)
	(printout t " Hello! Hope you are doing GREAT! Welcome to Complete Dog owner's Guide" crlf)
	(printout t " I'll need to take some information to give you the best guide for your canine friend!!! " crlf)
	(printout t crlf "**************************************************************************************************" crlf crlf)
	(printout t "Okay so let start with your name. Enter your name? (You can use alias instead of real name)" crlf)
    (bind ?*user* (read))
    (printout t crlf)
    
    (printout t "Soo "?*user*" what is your dog's name? (You can use alias instead of real name)" crlf)
    (bind ?*dog* (read))
    (printout t crlf)
    
    (printout t "Woooww!! "?*dog*" is such a lively name!!" crlf)
    (printout t "Thank you for the information. Now I just need one more information before we get started with our Fuzzy variabes" crlf)
    (printout t "I won't let you enter wrong input"crlf"I will crash otherwise :P"crlf"So kindly make sure enter the Correct Range given from the options " crlf)

    
    )

;Question asked to user which are validated

(deffacts questions
    "The questions that are asked to user."
    (question (ident current) (type number)
        (text "What is current weight of your dog? (Numeric Value Only)"))
    (question (ident age) (type l-m-h)
        (text "What is age of your dog? For:RANGE |<2|2-12|12-240|"))
    (question (ident breed) (type breed-size)
        (text "What is the adult breed size of your dog (in kgs)? (Google if you're not aware) Range: |<10|10-25|25-40|"))
    (question (ident weight) (type l-m-h)
        (text "What is the weight of your dog (in kgs)? Range: |<25||25-40||40-70| "))
    (question (ident deworm) (type stage)
        (text "Till which stage is your dog dewormed (months)? Range: |<4||4-12||12-240| "))
    (question (ident vaccine) (type stage)
        (text "Till which stage is your dog vaccined (months)? Range: |<3||3-6||6-240| "))
    )

;Answer for the question
(defmodule request-dog-details)
(defrule request-current
    (declare (salience 31))
    =>
    (assert (ask current)))

(defrule request-age
    (declare (salience 30))
    =>
    (assert (ask age)))

(defrule request-breed
    (declare (salience 29))
    =>
    (assert (ask breed)))

(defrule request-weight
    (declare (salience 28))
    =>
    (assert (ask weight)))

(defrule request-deworm
    (declare (salience 27))
    =>
    (assert (ask deworm)))

(defrule request-vaccine
    (declare (salience 26))
    =>
    (assert (ask vaccine)))


;asserting  the user data.
 (defrule assert-dog-fact   
    (answer (ident current) (text ?c))
    (answer (ident age) (text ?a))
    (answer (ident breed) (text ?b))
    (answer (ident weight) (text ?w))
    (answer (ident deworm) (text ?d))
    (answer (ident vaccine) (text ?v))
    =>
    (bind ?*weight* ?c)
    (bind ?*dog_age_user* ?a)
    (bind ?*dog_breed_user* ?b)
    (bind ?*dog_weight_user* ?w)
    (bind ?*dog_deworm_user* ?d)
    (bind ?*dog_vaccine_user* ?v)    
    (assert (user-dog-age (new FuzzyValue ?*dog_age* ?*dog_age_user*)))
  	(assert (user-dog-breed (new FuzzyValue ?*dog_breed* ?*dog_breed_user*)))
	(assert (user-dog-weight (new FuzzyValue ?*dog_weight* ?*dog_weight_user*)))
    (assert (user-dog-deworm (new FuzzyValue ?*dog_deworm* ?*dog_deworm_user*)))
    (assert (user-dog-vaccine (new FuzzyValue ?*dog_vaccine* ?*dog_vaccine_user*)))    
    (printout t crlf)

)

;Initialize all the global variables.
(defrule initialize-fuzzy-variables
    (declare (salience 100))  
    =>
    (printout t crlf "FUZZY INITIALIZING STARTING ......." crlf crlf)
    (printout t " ***Initializing Dog's Age*** " crlf)
    (?*dog_age* addTerm "low" (new ZFuzzySet 1 3))
	(?*dog_age* addTerm "medium" (new TriangleFuzzySet 7 5))
    (?*dog_age* addTerm "high" (new SFuzzySet 12 240))

    
    (printout t " ***Initializing Dog's Breed Size*** " crlf)
    (?*dog_breed* addTerm "small" (new ZFuzzySet 1 10))
	(?*dog_breed* addTerm "medium" (new TriangleFuzzySet 17 8))
    (?*dog_breed* addTerm "large" (new SFuzzySet 25 40))

    (printout t " ***Initializing Dog's Weight *** " crlf)
 	(?*dog_weight* addTerm "low" (new ZFuzzySet 1 25))
	(?*dog_weight* addTerm "medium" (new TriangleFuzzySet 33 8))
    (?*dog_weight* addTerm "high" (new SFuzzySet 40 70))
    
    (printout t " ***Initializing Dog's Deworming Stage *** " crlf)
 	(?*dog_deworm* addTerm "one" (new ZFuzzySet 1 4))
	(?*dog_deworm* addTerm "two" (new TriangleFuzzySet 7 5))
    (?*dog_deworm* addTerm "three" (new SFuzzySet 12 240))  
    
    (printout t " ***Initializing Dog's Vaccination Stage *** " crlf)
    (?*dog_vaccine* addTerm "one" (new ZFuzzySet 1 3))
	(?*dog_vaccine* addTerm "two" (new TriangleFuzzySet 4 2))
    (?*dog_vaccine* addTerm "three" (new SFuzzySet 6 240))   
    
    
    (printout t crlf crlf"FUZZY INITIALIZATION COMPLETE !!" crlf crlf)  
)

; Rule for dog

;Growth Stage of dog

(defrule rule-1
    "Age us less than 2 months"
    (declare (salience 18))
    (user-dog-age ?a&:(fuzzy-match ?a "low"))
      =>
	(printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following information tells you about your dog's stage:" crlf crlf)
	(printout t "Firstly it's important to know that the dog is still a puppy and less than 2 months and not reached weaning stage."crlf"You need to keep your dog on mother's milk" crlf)
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-2
    "Age from 3 to 12 months"
    (declare (salience 17))
    (user-dog-age ?a&:(fuzzy-match ?a "medium"))
      =>
	(printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following information tells you about your dog's stage:" crlf crlf)
	(printout t "Firstly it's important to know that your dog is currently in Weaning stage which begins after 2 months"crlf"You can now start to feed him dry food and take him off mother's milk" crlf)
    (printout t crlf "Once you finish reading press a CHARACTER and then ENTER to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-3
    "Age above 12 months"
    (declare (salience 16))
    (user-dog-age ?a&:(fuzzy-match ?a "high"))
      =>
	(printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following information tells you about your dog's stage:" crlf crlf) 
	(printout t "Your dog is has already reached an Adult stage" crlf)
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )


;Weight recommended and difference

(defrule rule-4
    "Recommended weight for age less than 2 months"
    (declare (salience 15))
    (user-dog-age ?a&:(fuzzy-match ?a "low"))
	(user-dog-breed ?b)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following tells you about your dog's recommened weight:" crlf crlf) 
	(if (fuzzy-match ?b "small") then
        (printout t "Your dog is a small breed and is less than 2 months old."crlf"The recommended weight is 3 kgs.")
        (bind ?*recommend* 3)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (fuzzy-match ?b "medium") then
        (printout t "Your dog is a medium breed and is less than 2 months old."crlf"The recommended weight is 5 kgs.")
        (bind ?*recommend* 5)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (fuzzy-match ?b "large") then
        (printout t "Your dog is a large breed and is less than 2 months old."crlf"The recommended weight is 9 kgs.")
        (bind ?*recommend* 9)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    	(printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    	(bind ?*next* (read t))
    )




(defrule rule-5
    "If age is above 2 to 12 months"
    (declare (salience 14))
    (user-dog-age ?a&:(fuzzy-match ?a "medium"))
	(user-dog-breed ?b)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following tells you about your dog's recommened weight:" crlf crlf) 
	(if (fuzzy-match ?b "small") then
        (printout t "Your dog is a small breed and is between 2 to 12 months old."crlf"The recommended weight is 11 kgs.")
        (bind ?*recommend* 11)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (fuzzy-match ?b "medium") then
        (printout t "Your dog is a medium breed and is between 2 to 12 months old."crlf"The recommended weight is 22 kgs.")
		(bind ?*recommend* 22)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (fuzzy-match ?b "large") then
        (printout t "Your dog is a large breed and is between 2 to 12 months old."crlf"The recommended weight is 35 kgs.")
        (bind ?*recommend* 35)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    	(printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    	(bind ?*next* (read t))
    )

(defrule rule-6
    "If age is more than 12 months"
    (declare (salience 13))
    (user-dog-age ?a&:(fuzzy-match ?a "high"))
	(user-dog-breed ?b)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following tells you about your dog's recommened weight:" crlf crlf) 
	(if (fuzzy-match ?b "small") then
        (printout t  "Your dog is a small breed and above 12 months ."crlf"The recommended weight is 15 kgs.")
		(bind ?*recommend* 15)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (fuzzy-match ?b "medium") then
        (printout t "Your dog is a medium breed and above 12 months old."crlf"The recommended weight is 29 kgs.")
		(bind ?*recommend* 29)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (if (fuzzy-match ?b "large") then
        (printout t "Your dog is a large breed and above 12 months old."crlf"The recommended weight is 48 kgs.")
		(bind ?*recommend* 48)
    	(if(> ?*recommend* ?*weight*) then
        	(printout t crlf "Weight less by "(- ?*recommend* ?*weight*)" kgs."))
        (if(< ?*recommend* ?*weight*) then
        	(printout t crlf "Weight more by "(- ?*weight* ?*recommend*)" kgs."))
        (if(eq ?*recommend* ?*weight*) then
        	(printout t crlf "Weight equal to recommended weight"crlf))
        )
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )


;Food quantity chart
(defrule rule-7
    "If age is till 2 months"
    (declare (salience 12))
    (user-dog-age ?a&:(fuzzy-match ?a "low"))
	(user-dog-breed ?b)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food quantity:" crlf) 
	(printout t "Since Your Dog is in weaning stage it needs a lot of food" crlf)
    (printout t crlf "FUN FACT: Puppies need more food than an adult dog") 
    (if (fuzzy-match ?b "small") then
        (printout t crlf crlf"Your dog is a small breed and less than 2 months old."crlf"The recommended food quantity is 0.44 kgs per day."crlf crlf))
    (if (fuzzy-match ?b "medium") then
        (printout t crlf crlf "Your dog is a medium breed and less than 2 months old."crlf"The recommended food quantity is 0.97 kgs per day."crlf crlf))
    (if (fuzzy-match ?b "large") then
        (printout t crlf crlf "Your dog is a large breed and less than 2 months old."crlf"The recommended food quantity is 1.42 kgs per day."crlf crlf))
    (printout t "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-8
    "If age is 2 to 12 months"
    (declare (salience 11))
    (user-dog-age ?a&:(fuzzy-match ?a "medium"))
	(user-dog-breed ?b)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food quantity:" crlf crlf) 
	(printout t "Since Your Dog is in Growing stage and needs to eat in the recommend quantity to avoid getting overweight at adulthood" crlf)
    (printout t crlf "FUN FACT: Puppies need more food than an adult dog") 
    (if (fuzzy-match ?b "small") then
        (printout t crlf crlf "Your dog is a small breed and is between 3 to 12 months old."crlf"The recommended food quantity is 0.32 kgs per day."crlf crlf))
    (if (fuzzy-match ?b "medium") then
        (printout t crlf crlf "Your dog is a medium breed and is between 3 to 12 months old."crlf"The recommended food quantity is 0.65 kgs per day."crlf crlf))
    (if (fuzzy-match ?b "large") then
        (printout t crlf crlf "Your dog is a large breed and is between 3 to 12 months old."crlf"The recommended food quantity is 1.04 kgs per day."crlf crlf))
    (printout t "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-9
    "If age above 12 months"
    (declare (salience 10))
    (user-dog-age ?a&:(fuzzy-match ?a "high"))
	(user-dog-breed ?b)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food quantity:" crlf crlf) 
	(printout t "Since Your Dog is in Adult and needs to have a balanced diet" crlf)
    (printout t crlf "FUN FACT: Puppies need more food than an adult dog") 
    (if (fuzzy-match ?b "small") then
        (printout t crlf crlf "Your dog is a small breed and is above 12 months old."crlf"The recommended food quantity is 0.29 kgs per day."crlf crlf))
    (if (fuzzy-match ?b "medium") then
        (printout t crlf crlf"Your dog is a medium breed and is above 12 months old."crlf"The recommended food quantity is 0.57 kgs per day."crlf crlf))
    (if (fuzzy-match ?b "large") then
        (printout t crlf crlf"Your dog is a large breed and is above 12 months old."crlf"The recommended food quantity is 0.95 kgs per day."crlf crlf))
    (printout t "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )


;Food Number of time per day

(defrule rule-10
    "If breed is small"
    (declare (salience 9))
    (user-dog-breed ?b&:(fuzzy-match ?b "small"))
	(user-dog-age ?a)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "It is also important to know how many time a day to feed your dog" crlf)
    (printout t crlf "A puppy eats more times a day than an adult dog" crlf "Reason being that puppy's digestive system is still not fully developed" crlf)
	(printout t crlf "NOTE: You need to equally divide the recommend food quantity per day into the number of times feeding is required." crlf)
    (printout t crlf "Your total intake should not cross recommended food quantity (kgs per day)"crlf)
    (if (fuzzy-match ?a "low") then
        (printout t  "Your dog is a small breed and is still puppy less than 2 months."crlf"I recommed feeding your dog THREE TO FOUR TIMES a day"crlf))
    (if (fuzzy-match ?a "medium") then
        (printout t "Your dog is a small breed and is between 2 to 12 months old."crlf"I recommed feeding your dog THREE TIMES a day"crlf))
    (if (fuzzy-match ?a "high") then
        (printout t "Your dog is a small breed and is above 12 months "crlf"I recommed feeding your dog TWO TIMES a day"crlf))
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )


(defrule rule-11
    "If breed is medium"
    (declare (salience 8))
    (user-dog-breed ?b&:(fuzzy-match ?b "medium"))
	(user-dog-age ?a)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "It is also important to know how many time a day to feed your dog" crlf)
    (printout t crlf "A puppy eats more times a day than an adult dog" crlf "Reason being that puppy's digestive system is still not fully developed" crlf)
	(printout t crlf "NOTE: You need to equally divide the recommend food quantity per day into the number of times feeding is required." crlf)
    (printout t crlf "Your total intake should not cross recommended food quantity (kgs per day)"crlf)
    (if (fuzzy-match ?a "low") then
        (printout t  "Your dog is a medium breed and is still puppy less than 2 months."crlf"I recommed feeding your dog THREE TO FOUR TIMES a day"crlf))
    (if (fuzzy-match ?a "medium") then
        (printout t "Your dog is a medium breed and is between 2 to 12 months old."crlf"I recommed feeding your dog THREE TIMES a day"crlf))
    (if (fuzzy-match ?a "high") then
        (printout t "Your dog is a medium breed and is above 12 months old."crlf"I recommed feeding your dog TWO TIMES a day"crlf))
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

(defrule rule-12
    "If breed is large"
    (declare (salience 7))
    (user-dog-breed ?b&:(fuzzy-match ?b "large"))
	(user-dog-age ?a)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "It is also important to know how many time a day to feed your dog" crlf)
    (printout t crlf "A puppy eats more times a day than an adult dog" crlf "Reason being that puppy's digestive system is still not fully developed" crlf)
	(printout t crlf "NOTE: You need to equally divide the recommend food quantity per day into the number of times feeding is required." crlf)
    (printout t crlf "Your total intake should not cross recommended food quantity (kgs per day)"crlf)
    (if (fuzzy-match ?a "low") then
        (printout t  "Your dog is a large breed and is still puppy less than 2 months."crlf"I recommed feeding your dog THREE TO FOUR TIMES a day"crlf))
    (if (fuzzy-match ?a "medium") then
        (printout t "Your dog is a large breed and is between 2 to 12 months old."crlf"I recommed feeding your dog THREE TIMES a day"crlf))
    (if (fuzzy-match ?a "high") then
        (printout t "Your dog is a large breed and is above 12 months old."crlf"I recommed feeding your dog TWO TIMES a day"crlf))
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )


; Recommended food brands


(defrule rule-13
    "Less than 2 months"
    (declare (salience 6))
    (user-dog-breed ?b)
	(user-dog-age ?a&:(fuzzy-match ?a "low"))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food brands:" crlf crlf) 
	(printout t "Since Your Dog is not an adult and needs to get nutrients essential from weaning till adult stage" crlf)
    (printout t crlf "Most dogs food companies manufacture food for both puppy and adult dog") 
    (if (fuzzy-match ?b "small") then
        (printout t crlf crlf "As your dog is still 2 months and small breed following are some recommended milk powder brands for your dog:"crlf"1)Pet Ag Esbilac Puppy Milk Replacer Small Breed"crlf"2)GNC Pets Ultra Mega Premium Milk Replacer Puppy Formula for Small Breed Puppies"crlf"3)21st Century Essential Pet Milk Replacer for Small Breed Puppy"crlf))
    (if (fuzzy-match ?b "medium") then
        (printout t crlf crlf "As your dog is still 2 months and medium breed following are some recommended milk powder brands for your dog:"crlf"1)Pet Ag Esbilac Puppy Milk Replacer Medium Breed"crlf"2)GNC Pets Ultra Mega Premium Milk Replacer Puppy Formula for Medium Breed Puppies"crlf"3)21st Century Essential Pet Milk Replacer for Medium Breed Puppy"crlf))
    (if (fuzzy-match ?b "large") then
        (printout t crlf crlf "As your dog is still 2 months and large breed following are some recommended milk powder brands for your dog:"crlf"1)Pet Ag Esbilac Puppy Milk Replacer Large Breed"crlf"2)GNC Pets Ultra Mega Premium Milk Replacer Puppy Formula for Large Breed Puppies"crlf"3)21st Century Essential Pet Milk Replacer for Large Breed Puppy"crlf))
    
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )



(defrule rule-14
    "If age is 2 to 12 months"
    (declare (salience 5))
    (user-dog-breed ?b)
	(user-dog-age ?a&:(fuzzy-match ?a "medium"))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food brands:" crlf crlf) 
	(printout t "Since Your Dog is not an adult and needs to get nutrients essential from weaning till adult stage" crlf)
    (printout t crlf "Most dogs food companies manufacture food for both puppy and adult dog") 
    (if (fuzzy-match ?b "small") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)Wellness Complete Health Small Breed Natural Dry Puppy Food"crlf"2)Blue Buffalo Freedom Grain-Free Chicken Recipe for Small Breed Puppies"crlf"3)Holistic Select Natural Dry Dog Food for Small Breed Puppy"crlf))
    (if (fuzzy-match ?b "medium") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)Taste of the Wild Grain-Free High Prairie Medium Puppy Formula Dry Food"crlf"2)Wellness Complete Health Medium Puppy Recipe"crlf"3)Blue Buffalo Basics Limited-Ingredient Dry Medium Puppy Food"crlf))
    (if (fuzzy-match ?b "large") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)Wellness Complete Health Natural Dry Large Breed Puppy Food, Chicken, Salmon & Rice"crlf"2)Nutro Max Natural Large Breed Puppy Dry Dog Food"crlf"3)Holistic Select Natural Dry Food Large Breed Puppy Health"crlf))
    
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )



(defrule rule-15
    "If age is 12 to more"
    (declare (salience 4))
    (user-dog-breed ?b)
	(user-dog-age ?a&:(fuzzy-match ?a "high"))
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "The following diet chart provides you with recommended food brands:" crlf crlf) 
	(printout t "Since Your Dog is an adult and needs diet which helps to stay fit and active" crlf)
    (printout t "Most dogs food companies manufacture food for both puppy and adult dog") 
    (if (fuzzy-match ?b "small") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)Wellness Complete Health Small Breed"crlf"2)Wellness Simple Grain-Free Small Breed"crlf"3)Blue BuffaloLife Protection Small Breed"crlf))
    (if (fuzzy-match ?b "medium") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)ACANA Wild Prairie Regional Formula Grain-Free Dry Medium Dog Food"crlf"2)Victor Yukon River Salmon & Sweet Potato Grain-Free Dry Medium Dog Food"crlf"3)Merrick’s Grain-Free Real Buffalo and Sweet Potato Recipe Medium Breed"crlf))
    (if (fuzzy-match ?b "large") then
        (printout t crlf crlf "Following are some recommended brand for your dog:"crlf"1)ACANA Wild Prairie Regional Formula Grain-Free Dry Large Dog Food"crlf"2)Victor Yukon River Salmon & Sweet Potato Grain-Free Dry Large Dog Food"crlf"3)Fromm Large Breed Adult Gold"crlf))
    
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )

;Life expectancy

(defrule rule-16
    "Life expectancy according to breed"
    (declare (salience 3))
    (user-dog-breed ?b)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "A sad fact but let me tell you about dog's life expectancy" crlf)
	(if (fuzzy-match ?b "small") then
        (printout t crlf" The life expectancy of your small dog breed is 15 to 16 years "crlf))    
    (if (fuzzy-match ?b "medium") then
        (printout t crlf"The life expectancy of your dog is 11 to 13 years "crlf))
    (if (fuzzy-match ?b "large") then
        (printout t crlf"The life expectancy of your dog is 10 to 12 years "crlf))
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    )


;Deworming

(defrule rule-17
    "Deworming according to age"
    (declare (salience 2))
    (user-dog-deworm ?d)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "Worming is a very serious issue in dogs." crlf)
    (printout t crlf "Deworming of dog is essential to make sure it recieves all the nutrients properly as worms in dog can consume the food leading to underweight." crlf)
    (printout t "Good to know you have dewormed your dog."crlf"But you still need to do deworming again at the following intervals")
    (if(fuzzy-match ?d "one") then
        	(printout t crlf "1)You need to deworm your dog at 6th week and 8th week"crlf"2)Then again at 3rd and 4th month"crlf"3)Again at 6th and 12th month"crlf"4)After that once every Year"))
        (if(fuzzy-match ?d "two") then
        	(printout t crlf "1)You need to deworm your dog again at 6th and 12th month"crlf"2)After that once every Year"))
        (if(fuzzy-match ?d "three") then
        	(printout t crlf "1)As you dog is adult you need to deworm your dog after once every year"))
        
        (printout t crlf"If you have still not dewormed your dog, I recommend to visit the VET ASAP and start with deworming."crlf)

    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
)
    
    
;Vaccination

(defrule rule-18
    "Vaccination according to age"
    (declare (salience 1))
    (user-dog-vaccine ?v)
    =>
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "Vaccination makes sure to keep your dog's immunity system at it's best." crlf)
    (printout t crlf "Vaccination chart is given below:" crlf)
    (printout t "Good to know you have given vaccination to your dog."crlf"But you still need to do vaccination again at the following intervals")
    	(if(fuzzy-match ?v "one") then
        	(printout t crlf "You need to give next vaccination to your dog by 3rd Month:"crlf"NAME OF VACCINATION: DHPP (vaccines for distemper, adenovirus [hepatitis], parainfluenza, and parvovirus)"crlf"Then again at 6th month of :"crlf"NAME OF VACCINATION: Rabies, DHPP"crlf"Again after 12th month you should give:"crlf"NAME OF VACCINATION: DHPP- Every 1 - 2 Years"crlf"Rabies- Every 1-3 Years"))
        (if(fuzzy-match ?v "two") then
        	(printout t crlf "You need to give next vaccination again at 6th month of :"crlf"NAME OF VACCINATION: Rabies, DHPP"crlf"Again after 12th month you should give:"crlf"NAME OF VACCINATION: DHPP- Every 1 - 2 Years"crlf"Rabies- Every 1-3 Years"))
        (if(fuzzy-match ?v "three") then
        	(printout t crlf "After 12th month you should give:"crlf"NAME OF VACCINATION: DHPP- Every 1 - 2 Years"crlf"Rabies- Every 1-3 Years"))
        (printout t crlf "If you have not started with vacciation, I recommend to visit the VET ASAP and start with vaccination."crlf)
        
    (printout t crlf "Once you finish reading press any CHARACTER on keyboard and then press ENTER key to go to next information:")
    (bind ?*next* (read t))
    
    
    (printout t crlf "Thank You for your time.")
    (printout t crlf "If your are using or following a different way than anything mentioned above kindly confirm with your VET")
    (printout t crlf "HOPE we meet Again!! END")
    (printout t crlf "*********************************************************************************************************")
    (printout t crlf "*********************************************************************************************************")
    
    )


(deffunction run-application ()
    (reset)
    (focus start request-dog-details)
    (run))

(while TRUE 
    (run-application))
