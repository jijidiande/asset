(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)
(*
   The class A2I provides integer-to-string and string-to-integer
conversion routines.  To use these routines, either inherit them
in the class where needed, have a dummy variable bound to
something of type A2I, or simpl write (new A2I).method(argument).
*)


(*
   c2i   Converts a 1-character string to an integer.  Aborts
         if the string is not "0" through "9"
*)
class A2I {

   c2i(char : String) : Int {
 if char = "0" then 0 else
 if char = "1" then 1 else
 if char = "2" then 2 else
      if char = "3" then 3 else
      if char = "4" then 4 else
      if char = "5" then 5 else
      if char = "6" then 6 else
      if char = "7" then 7 else
      if char = "8" then 8 else
      if char = "9" then 9 else
      { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
      fi fi fi fi fi fi fi fi fi fi
   };

(*
 i2c is the inverse of c2i.
*)
   i2c(i : Int) : String {
 if i = 0 then "0" else
 if i = 1 then "1" else
 if i = 2 then "2" else
 if i = 3 then "3" else
 if i = 4 then "4" else
 if i = 5 then "5" else
 if i = 6 then "6" else
 if i = 7 then "7" else
 if i = 8 then "8" else
 if i = 9 then "9" else
 { abort(); ""; }  -- the "" is needed to satisfy the typchecker
      fi fi fi fi fi fi fi fi fi fi
   };

(*
 a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic 
overflow.

*)
   a2i(s : String) : Int {
      if s.length() = 0 then 0 else
 if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
      if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
         a2i_aux(s)
      fi fi fi
   };

(*
a2i_aux converts the usigned portion of the string.  As a programming
example, this method is written iteratively.
*)
   a2i_aux(s : String) : Int {
 (let int : Int <- 0 in	
         {	
             (let j : Int <- s.length() in
           (let i : Int <- 0 in
        while i < j loop
       {
           int <- int * 10 + c2i(s.substr(i,1));
           i <- i + 1;
       }
        pool
      )
        );
            int;
     }
      )
   };

(*
  i2a converts an integer to a string.  Positive and negative 
numbers are handled correctly.  
*)
  i2a(i : Int) : String {
 if i = 0 then "0" else 
      if 0 < i then i2a_aux(i) else
        "-".concat(i2a_aux(i * ~1)) 
      fi fi
  };
 
(*
  i2a_aux is an example using recursion.
*)		
  i2a_aux(i : Int) : String {
      if i = 0 then "" else 
     (let next : Int <- i / 10 in
    i2a_aux(next).concat(i2c(i - next * 10))
     )
      fi
  };

};

class Stack inherits IO{
stack:String<-"";
tos : Int <-0;
isNil():Bool{
if tos = 0 then true else false fi
};
(*增加返回值*)
push(x:String):Object{
{
stack <- stack.concat(x);
tos <- tos+1;
}
};
pop():String{
--1
--tos=1
--12345 要想拿到5 substr(4,1)
--tos=5 tos-1是栈顶元素
--(tos-1,1)从tos-1开始即栈顶第一个 步长为1
-- from index i with length l.
let sc : String <- stack.substr(tos-1, 1) in {stack <- stack.substr(0,tos-1);tos <- tos -1; sc;}--tos是0则有一个元素，Sc是""//返回Sc
-- stack.substr(0,tos-2);
};
top() :String{
stack.substr(tos-1,1)
};
(*12345
0,4
tos is 5 *)
display_stack():Object{
--       out_string(stack)
-- };
   let len : Int <- stack.length() in{
      while 0<len 
      loop
      {
         out_string(stack.substr(len-1,1));--inherits IO 的作用在这
         len <- len - 1;
         out_string("\n");
      }
      pool;
     }
};
};
class Main inherits IO {
   display_promt():String{
{
   out_string(">");
   in_string();
}
   };
   main() : Object {
(let z : A2I <- new A2I  , stack : Stack <- new Stack in
      -- stack := new Stack();
while true loop
(let input : String <- display_promt() in 
--input <- IO.in_string()
if input = "x" then abort() else 
if input = "d" then 
-- out_string(stack) 
stack.display_stack()  else 
if input = "e" then 
{
   if stack.isNil() then out_string("") else
-- if stack.substr(tos-1,1) = "+" then 

if stack.top()="+" then
{
-- stack.pop();
-- temp <- i2a(a2i(stack.substr(tos-1,1))+a2i(stack.substr(tos-2,1))) 
-- tos <-tos-1
-- stack.pop()
-- tos <-tos-1
-- stack.pop()
-- tos <-tos-1
-- stack.concat(temp) tos <- tos+1 
stack.pop();
let a : Int <- new Int, b : Int <- new Int in 	
{
   a  <- z.a2i(stack.pop());
   b  <- z.a2i(stack.pop());
   a <- a + b;
   stack.push(z.i2c(a));
};
}else if stack.top()="s" then {
stack.pop();
let temp1 : String <- new String , temp2 : String <- new String in
{
temp1 <- stack.pop();
temp2 <- stack.pop();
stack.push(temp1);
stack.push(temp2);
};
}
else
-- { abort(); ""; }
out_string("")
fi 
fi
fi;
}
else 
-- {abort();"";} 
stack.push(input)
fi
fi
fi
)
pool
)
};
};