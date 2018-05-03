#Instruction to Run the application
1. Extract 2016CSJ0049.tgz
2. Go to 2016CSJ0049 folder.
3. Open 2016CSJ0049(Starter).sml file and In first line replace "t.sml" with the filename of your BigInt structure along with the file for signature of bignat, structure of bignat.(If needed add open command for structure of BigInt in 2nd line).
4. Open terminal and execute "make" in terminal.
5. Interpreter for Calculator program has started.
6. '->' It show that here you can enter your query.
	For Example,
		**-> 10+100-20**
	"->" will be displayed by the calculator you have to enter your input in the same line.
	Answer to your query will be generated in next line as
		**=90**
	After that Calculator will ask you if you want to continue to want to exit.
	To exit,write "quit" (Without quotes,here quotes are only for explaining purpose.) else to continue press enter.
7. Calculator uses BigInt so any larger number can be entered in query.

##Operators
For input

###Note:
It was mentioned in the assignment to take Bigint's function as they are and not change there name.
For giving input in the calculator writing 4++5--2 was not a good idea, so for giving input I'm using +,-,* and / symbol.(All the functions of bigint are still with same name.I'm not making any changes to them).


**Infix Operator**
	"+" Denotes Addition
    "-" Denotes Substraction
	Asterisk "\*" Denotes Multiplication
	"/" Denotes Division(In bigint it return both quotient and remainder but here it is only supposed to return quo)
	== Denotes Equals function it returns 0 for false and 1 for true
	<< Denotes Less than function it returns 0 for false and 1 for true
	>> Denotes greater than function it returns 0 for false and 1 for true
	<<= Denotes Less than equal to function it returns 0 for false and 1 for true
	>>= Denotes Greater than equal to function it returns 0 for false and 1 for true

##Grammar

<{E,Q,T,D,K,F},{+,-,\*,/,number,(,)},P,{E}>

E--> Q+E | Q
Q--> T-Q | T
T--> D*T | D
D--> K/D | K
K--> (E) | F
F--> -F | number


This grammar takes care of Precedence automatically and it is unambiguous.
Abstract Syntax tree is generated after this.

Using DataType:

datatype expression =None 
	| Bracket of expression 
	| Digit of bigint 
	| Add of expression\*expression 
	| Sub of expression\*expression 
	| Mul of expression\*expression 
	| Div of expression\*expression
	| Equal of expression\*expression
	| Lessthan of expression\*expression
	| LessthanEqual of expression\*expression
	| Greaterthan of expression\*expression
	| GreaterthanEqual of expression\*expression

After this based on the type of expression it is evaluated. 