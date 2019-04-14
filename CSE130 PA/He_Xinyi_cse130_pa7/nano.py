#!/usr/bin/env python3
from nano import *
from misc import Failure

# prologable interface
class Prologable():
    """This is a function in the interface, so every class wichi
    attends this will have thia function toProlog."""
    def toProlog(self) -> str:
        raise Failure("SHOULD NOT GET HERE -- subclasses should override")

    def __eq__(self, other):
        """This function __eq__ checks whether two objects which
        extends the interface are equal."""
        if isinstance(other, Prologable):
            return self.toProlog() == other.toProlog()
        else:
            return False

    def __str__(self):
        """The __str__ function everytime called the toProlog function of the class."""
        return self.toProlog()


# expression interface
class Expression(Prologable):
    """This function defines that all the Expression object are Prologable."""
    def toProlog(self):
        return

# binop interface
class Bop(Prologable):
    """This function defines that all the Bop object are Prologable. """
    def toProlog(self):
        return

class Plus(Bop):
    """toProlog takes no argument and returns the string plus."""
    def toProlog(self):
        return 'plus'

class Minus(Bop):
    """toProlog takes no argument and returns the string minus."""
    def toProlog(self):
        return 'minus'

class Mul(Bop):
    """toProlog takes no argument and returns the string mul."""
    def toProlog(self):
        return 'mul'

class Div(Bop):
    """toProlog takes no argument and returns the string div."""
    def toProlog(self):
        return 'div'

class Eq(Bop):
    """toProlog takes no argument and returns the string eq."""
    def toProlog(self):
        return 'eq'

class Neq(Bop):
    """toProlog takes no argument and returns the string neq."""
    def toProlog(self):
        return 'neq'

class Lt(Bop):
    """toProlog takes no argument and returns the string lt."""
    def toProlog(self):
        return 'lt'

class Leq(Bop):
    """toProlog takes no argument and returns the string leq."""
    def toProlog(self):
        return 'leq'

class And(Bop):
    """toProlog takes no argument and returns the string and."""
    def toProlog(self):
        return 'and'

class Or(Bop):
    """toProlog takes no argument and returns the string or."""
    def toProlog(self):
        return 'or'

class Cons(Bop):
    """toProlog takes no argument and returns the string cons."""
    def toProlog(self):
        return 'cons'

# Expressions
class Const(Expression):
    def __init__(self, i: int):
        self.v = i
    def toProlog(self):
        """toProlog takes no argument and return the string in the form const()"""
        return 'const('+str(self.v)+')'

class Bool(Expression):
    def __init__(self, b: bool):
        self.v = b
    def toProlog(self):
        """toProlog takes no argument and return the string in the form boolean()"""
        return 'boolean('+str(self.v).lower()+')'

class NilExpr(Expression):
    def __init__(self):
        return
    def toProlog(self):
        """toProlog takes no argument and return the string nil"""
        return 'nil'

class Var(Expression):
    def __init__(self, v: str):
        self.v = v

    def toProlog(self):
        """toProlog takes no argument and return the string in the form var()"""
        return 'var('+self.v+')'

class Bin(Expression):
    def __init__(self, l: Expression, o: Bop, r:Expression):
        self.l = l
        self.r = r
        self.o = o
    def toProlog(self):
        """toProlog takes no argument and return the string in the form bin(,,)"""
        return 'bin('+self.l.toProlog()+', '+self.o.toProlog()+', '+self.r.toProlog()+')'

class If(Expression):
    def __init__(self, c: Expression, t: Expression, f: Expression):
        self.c = c
        self.t = t
        self.f = f
    def toProlog(self):
        """toProlog takes no argument and return the string in the form ite(,,)"""
        return 'ite('+self.c.toProlog()+', '+self.t.toProlog()+', '+self.f.toProlog()+')'

class Let(Expression):
    def __init__(self, v: str, e: Expression, body: Expression):
        self.v = v
        self.e = e
        self.body = body
    def toProlog(self):
        """toProlog takes no argument and return the string in the form let(,,)"""
        return 'let('+self.v+', '+self.e.toProlog()+', '+self.body.toProlog()+')'

class Letrec(Expression):
    def __init__(self, v: str, e: Expression, body: Expression):
        self.v = v
        self.e = e
        self.body = body
    def toProlog(self):
        """toProlog takes no argument and return the string in the form letrec(,,)"""
        return 'letrec('+self.v+', '+self.e.toProlog()+', '+self.body.toProlog()+')'

class App(Expression):
    def __init__(self, f: Expression, arg: Expression):
        self.f = f
        self.arg = arg
    def toProlog(self):
        """toProlog takes no argument and return the string in the form app(,,)"""
        return 'app('+self.f.toProlog()+', '+self.arg.toProlog()+')'

class Fun(Expression):
    def __init__(self, v: str, body: Expression):
        self.v = v
        self.body = body
    def toProlog(self):
        """toProlog takes no argument and return the string in the form fun(,)"""
        return 'fun('+self.v+', '+self.body.toProlog()+')'

# Types

class Type(Prologable):
    def toProlog(self):
        """This function defines that all the Type object are Prologable."""
        return

class IntTy(Type):
    def __init__(self):
        return
    def toProlog(self):
        """toProlog takes no argument and return the string int."""
        return "int"

class BoolTy(Type):
    def __init__(self):
        return
    def toProlog(self):
        """toProlog takes no argument and return the string bool."""
        return "bool"

class ArrowTy(Type):
    def __init__(self, l: Type, r: Type):
        self.l = l
        self.r = r
    def toProlog(self):
        """toProlog takes no argument and return the string in form arrow(,)."""
        return 'arrow('+str(self.l.toProlog())+', '+str(self.r.toProlog())+')'

class ListTy(Type):
    def __init__(self, inner: Type):
        self.inner = inner
    def toProlog(self):
        """toProlog takes no argument and return the string in form list()."""
        return "list("+self.inner.toProlog()+')'

class VarTy(Type):
    def __init__(self, name: str):
        self.name = name
    """toProlog takes no argument and return the string of the variable name."""
    def toProlog(self):
        return self.name
