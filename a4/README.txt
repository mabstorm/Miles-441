Modules:

State.hs  -- implements the type Var and the type State, which maps variables to
	     integer values

Imp.hs    -- implements the syntax of: 
       	  	     * imperative programs (type Program),
		     * imperative states (assignment, loops, etc) (type Comm) 
          	     * boolean expressions (type BExp),
       	  	     * integer expressions (type Exp)
             and an evaluator for programs and a well-formedness checker

Prover.hs -- implements a theorem prover by converting BExp into
 	     presburger arithmetic formulae (see below)

Hoare.hs  -- implements the weakest pre-condition generator wp
	     and the program verifier verify

Play.hs   -- a module for testing code

PTest.hs  -- a standalone module that tests to make sure you are linking with
	     the library for Presburger arithmetic properly

Presburger Formulae:

For more information, see:  http://hackage.haskell.org/package/presburger
and especially:  http://hackage.haskell.org/packages/archive/presburger/0.4/doc/html/Data-Integer-Presburger-HOAS.html

To install the library, invoke in a shell:

  cabal install presburger

To use in a program, import as:

  import Data.Integer.Presburger

To construct formulae use the following constructors:

Formula :/\: Formula	 
Formula :\/: Formula	 
Formula :=>: Formula	 
Formula :<=>: Formula	 
Not Formula	 
Exists (Term -> Formula)	 
Forall (Term -> Formula)	 
TRUE	 
FALSE	 
Term :<: Term	 
Term :>: Term	 
Term :<=: Term	 
Term :>=: Term	 
Term :=: Term	 
Term :/=: Term	 
Integer :| Term

To check the validity of a formula run the following function:

check :: Formula -> Bool


