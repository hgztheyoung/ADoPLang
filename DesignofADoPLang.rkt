;All the design here is some naive s-exp version deriving from Dijkstra's tutorial language from A discipline of Programming.
;I may make some naive descisions,making the language different and probably worse.
;For those who are interested in the design here,I sincerely recommend you to read A Discipline of Programming by your own.
;This is just a design script,I may not be able to implement them.
;I know syntax is not that important,but I find trying to reduce parentheses a lot of fun. :D

;let there be names (and types and values)
(let ([x 3]
      [y 4])
  (+ x y))

;chapter 10 pri/glo/vir  and con/var
;chapter 11 Array variable,viz Dijkstra's vector
;pri create variable in this scope,pri is short for private.
;this looks like the original let in Racket.

;glo inherites varible from outside scope with initialized value.
;Explicitly tells which variables to use.While with Lexical scoping, we can use variables implicitly.
;By Explicitly saying which variables to use,we don't need to look outside to understand a inner scope.
;While for the outer scope,for a certain variable,to figure out How it is changed,we don't need to look inside a scope that
;reference that variable as a constant.
;glo is short for global

;vir inherits un-initialized variable from outside scope.
;And init it during in this scope (maybe in its inner scope,which is still in its scope.).
;vir is short for virgin.


;con is short for constant,var is short
;for variable



;let there be names and qulifiers .(without type and value.)
;and give a name its type and value during initilizing.
(let ([(a b c) (: pri con balabala ...)] ;inspired by typed racket,use : when a variable has more than one qulifier.
      [(d e) : pri] 
      [(f g) : pri con] ;less parentheses,harder to implement
      [h : pri var]
      [(x1 y1 z1) : pri] ; allowing default implicit qulifier,choosing it to be pri var.
      [(x2 y2 z2) : pri] ; with default qulifier,it looks "easier" and more confusing,with implicit "naturly" choosen qulifier.
      [(x3 y3 z3) : pri]
      [z4]
      [(x5 y5 z5)]      
      [(l1 l2)]
      ;or even
      [x6 x7]
      [x8 x9 (: pri con)]
      [x10 x11 : pri]
      )
  
  (init (x1 y1) [(3 4) . of . Int]) ; this . of . form reads like English,not sure whether its a good design
  (init (x2 y2 z2) [{3 4} [4 5] (5 6) . of . Point]) ; calling construct funtion of Point {} [] or () doesn't make a difference.
  
  (init (x3 y3) [: Point {3 4} [4 5]]) ; this is an alternative design saying the same thing.You may read the :  "to be"
                                       ; use : when declare many varibles the be the same type.
                                       ; just like Point x3{3,4},y3{4,5};
  (init (a b c) [: Int 1 2 3])
  (init z3 (Point 3 4))
  (init (l1 l2) [:Line (x3 y3) (x3 y3)]);x3,y3 alread inited, OK the use them here.
  ; all of the above init statments will result in a call to create a struct or class or prim type in Racket

  (init (x5 y5 z5) [(Int 3) (Boolean #f) #t])
  ;(Boolean seems foolish,we may infer that #f or #t can always be Boolean)
  ;this is a quite sofisticated problem,we don't know whether 3 is Int or Long.
  ;It remains a problem to me whether we should differ them.
  ;And I'm to naive to implement some type inference.
  ;Let's just care about syntax for now.
  ;Naively,we may think of 3 as (Int 3) and #t as (Boolean #t)
  ;Which I'm sure to cause a lot of trouble...
  
  (send l1 goThroughOrigin) ; this is just l1.goThroughOrigin()
  (send l1 moveUp! 1); this is l1.moveUp!(1),or with Dijkstra's manner, l1:moveUp(1).
  (+ x y)
  z4  ;referencing z4 here should cause an error,cause z hasn't been initialized
  (let ([z vir]
        [x pri] ;;OK to use pri x,if use globe x,wound bother to name a pri x
        [y glo]
        [(a b) (: glo const)]
        (init z ("213hgz" . of . String))
        (send z (get_i y)))))


;initilize with an iffi,resulting in or type
;Notice that a variable should be initilized in all the branches.
;as Dijkstra says,as long as the type can provide all the functions sent to it.
;When should not refuse to init it to be different types. (Dynamic type?I don't know...)
(let ([x (: pri con)])
  (iffi
   (#t (init x (Int 213)))
   (#t (init x #t)))
  ;x is Int or Boolean now
  (print x))

(let ([x (: pri con)])
  (iffi
   (#t (init x (HpPrinter "hp")))
   (#t (init x (CanonPrinter "Canon"))))
  ;x is HpPrinter or CanonPrinter now
  (send x print "Hello World" "default_Setting")
  (let ([x (: glo con)]);we can use x and we turn out just caring its qulifiers but its type,HpPrinter or CanonPrinter
    (send x print "Using OutSide x" "default_Setting")))
