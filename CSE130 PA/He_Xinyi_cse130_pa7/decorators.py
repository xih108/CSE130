#!/usr/bin/env python3
from misc import Failure

class profiled(object):
    def __init__(self,f):
        self.__count=0
        self.__f=f
        self.__name__=f.__name__
    def __call__(self,*args,**dargs):
        self.__count+=1
        return self.__f(*args,**dargs)
    def count(self):
        return self.__count
    def reset(self):
        self.__count=0

class traced(object):

    count = 0
    def __init__(self,f):
        self.__f = f
        self.__name__= f.__name__
        self.count = 0

    def __call__(self,*args,**kwargs):
        """Print a pipe symbol followed by a space ("| ") for every level of nested function calls.
           Print a comma then a minus sign then a space (",- ") next.
           Print the name of the function being traced followed by an open parenthesis followed by the repr() of all of the arguments. Arguments should be seperated by a comma followed by a space (", "). After the normal arguments, print all the keyword arguments in the form keyword then equals sign then repr() of the value of the keyword argument. The keyword arguments should also be seperated by a comma followed by a space. Keyword arguments should be printed in the order returnd by dict.items().
           Next increase the nesting level and call the function itself.
           At the original nesting level, print a pipe symbol followed by a space ("| ") for every level of nested function calls.
           Print a backquote then a minus sign then a space("`- ").
           Finally, print the repr() of the return value."""

        # upper pattern including all arguments
        upper = '| '*traced.count + ',- '+ self.__name__ + "("+', '.join([str(a) for a in args]) + ', '.join([str(f) + '=' + repr(s) for f,s in kwargs.items()])+')'
        print (upper)
        traced.count += 1 
        try:
            returnValue = self.__f(*args,**kwargs)
            traced.count -= 1
            # lower pattern including all the return value
            lower = '| '*traced.count + '`- '+ str(returnValue)
            print (lower)
            return returnValue     
        except Exception as e:
            traced.count -= 1
            raise e

    def reset(self):
        traced.count = 0
            

class memoized(object):
    def __init__(self,f):
        self.__name__= f.__name__
        self.__f = f
        self.__mem = {}
    
    def __call__(self,*args,**kwargs):
        """When the decorated function is called, the decorator should check 
           to see if the function has already been called with the given arguments. 
           If so, the decorator should return the value the the function returned 
           when it was last called with the given arguments. If the function last 
           threw an exception when called with the given arguments, the same exception should be thrown again. 
           If the function has not been called with the given arguments, then call it and record the return value or exception. 
           Then return the return value or raise the thrown exception."""
        
        # convert all arguments to string
        arguments = str(args) + str(list(sorted(kwargs.items())))
        
        # each time check whether such arguments appeared before
        if arguments not in self.__mem:
            try:
                value = self.__f(*args,**kwargs)
                self.__mem[arguments] = value
                return value
            except Exception as e:
                self.__mem[arguments] = e
                raise e
        else:
            if isinstance(self.__mem[arguments], Exception): 
                raise self.__mem[arguments]
            else: return self.__mem[arguments]

# run some examples.  The output from this is in decorators.out
def run_examples():
    for f,a in [(fib_t,(7,)),
                (fib_mt,(7,)),
                (fib_tm,(7,)),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp.reset,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (even_t,(6,)),
                (quicksort_t,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (change_t,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                ]:
        print("RUNNING %s(%s):" % (f.__name__,", ".join([repr(x) for x in a])))
        rv=f(*a)
        print("RETURNED %s" % repr(rv))

@traced
def fib_t(x):
    if x<=1:
        return 1
    else:
        return fib_t(x-1)+fib_t(x-2)

@traced
@memoized
def fib_mt(x):
    if x<=1:
        return 1
    else:
        return fib_mt(x-1)+fib_mt(x-2)

@memoized
@traced
def fib_tm(x):
    if x<=1:
        return 1
    else:
        return fib_tm(x-1)+fib_tm(x-2)

@profiled
@memoized
def fib_mp(x):
    if x<=1:
        return 1
    else:
        return fib_mp(x-1)+fib_mp(x-2)

@traced
def even_t(x):
    if x==0:
        return True
    else:
        return odd_t(x-1)

@traced
def odd_t(x):
    if x==0:
        return False
    else:
        return even_t(x-1)

@traced
def quicksort_t(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_t([x for x in l[1:] if x<pivot])
    right=quicksort_t([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

@traced
@memoized
def quicksort_mt(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_mt([x for x in l[1:] if x<pivot])
    right=quicksort_mt([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

class ChangeException(Exception):
    pass

@traced
def change_t(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_t(l[1:],a)
    else:
        try:
            return [l[0]]+change_t(l,a-l[0])
        except ChangeException:
            return change_t(l[1:],a)

@traced
@memoized
def change_mt(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_mt(l[1:],a)
    else:
        try:
            return [l[0]]+change_mt(l,a-l[0])
        except ChangeException:
            return change_mt(l[1:],a)


