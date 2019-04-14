from misc import Failure

# This class defines basic operations of Vector.
class Vector(object):

    # THis is the constructor of vector
    def __init__(self,value):
        """ The constructor take a single argument. 
            If this argument is an int or an instance of a class derived from int,  
            then create a Vector with the length with each element initialized to 0.0. 
            If the length is negative, raise a ValueError with an appropriate message. 
            If the argument is a sequence, then initialize with vector with the length and values of the given sequence. 
            Otherwise, raise a TypeError with an appropriate message."""
        if isinstance(value,int):
            if value >=0:
                self.value = [0.0]*value
            else :
                raise ValueError("Vector length cannot be negative")
        else :
            try :
                self.value = list(value)
            except :
                raise TypeError("The parameter has wrong type")

    # This is the __repr__ method
    def __repr__(self):
        """ The method return a string consist of the name of the class 
            followed by ( the contents of the vector represented as a list ). """
        return "Vector("+str(self.value)+")"

    # This is the __len__ method
    def __len__(self):
        """ The method return the length of the Vector."""
        return len(self.value)

    # This is the __iter__ method
    def __iter__(self):
        """ This method return an object that can iterate over the elements of the Vector."""
        for i in self.value:
            yield i

    # This is the __add__ method
    def __add__(v1,v2):
        """ This method return the addition of two same length vector.
            If the two objects have different lengths, then raise a ValueError with an appropriate message."""
        if len(v1) == len(v2):
            seq = [list(v1)[i]+list(v2)[i] for i in range(len(v1))]
            return Vector(seq)
        else:
            raise ValueError("Passes in vectors have different lengths")

    # This is the __radd__ method
    def __radd__(v1,v2):
        """ This method makes sure the commutative law of addition works properly"""
        if len(v1) == len(v2):
            seq = [list(v1)[i]+list(v2)[i] for i in range(len(v1))]
            return Vector(seq)
        else:
            raise ValueError("Passes in vectors have different lengths")

    # This is the dot method
    def dot(self,other):
        """ This method returns the dot product of a vector and a sequence.
            If the two objects have different lengths, then raise a ValueError with an appropriate message."""
        if len(self) == len(other):
            dot_prod = sum([list(self)[i]*list(other)[i] for i in range(len(self))])
            return dot_prod
        else:
            raise ValueError("Passes in vectors have different lengths")

    # This is the __getitem__ method
    def __getitem__(self,key):
        """ This method allow element level access to the Vector if key is an int in appropriate range.
            Otherwise, raise IndexError with an appropriate message.
            It also allow slice level access to the Vector if key is a slice object. """
        if isinstance(key,slice):
            return self.value[key]
        if isinstance(key,int):
            if key >=0 and key <len(self):
                return self.value[key]
            elif key<0 and key >= -len(self):
                return self.value[len(self)+key]
            else:
                raise IndexError("Index out of bound")

    # This is the __setitem__ method
    def __setitem__(self,key,value):
        """ This method set the value of the corresponding key to be the value passed in.
            If it doesn't preserve the length of the vector, then raise ValueError with  an appropriate message."""
        if isinstance(key,slice):
            if len(self[key]) == len(value):
                self.value[key] = value
            else:
                raise ValueError("Cannot change the length")

        if isinstance(key,int):
            if key >=0 and key <len(self):
                self.value[key] = value
            elif key<0 and key >=i -len(self):
                self.value[len(self)+key] = value
            else:
                raise IndexError("Index out of bound")

    # This is the __eq__ method
    def __eq__ (self,other):
        """ This method returns true if each element in the first Vector 
            is equal to the respective element in the second Vector. 
            Otherwise, return false."""
        if isinstance(other,Vector):
            return (self.value == other.value)
        return False

    # This is the __ne__method
    def __ne__ (self,other):
        """ This method returns the negation of __eq__. """
        return not(self == other)

    # This is the __gt__ method
    def __gt__ (self,other):
        """ This method returns true if the element of a is larger than the corresponding element of b 
            in descending soorted order ."""
        l1 = sorted(self.value,reverse = True)
        l2 = sorted(other.value,reverse = True)
        for i in range(len(l1)):
            if l1[i] > l2[i]:
                return True
        return False

    # This is the __ge__ method
    def __ge__(self,other):
        """ This method returns true if self is greater than other or self equals other in sorted order."""
        return (self > other) or (sorted(self) == sorted(other))

    # This is the __lt__ method
    def __lt__(self,other):
        """ This method returns true if other > self."""
        return other > self

    # This is the __le__ method
    def __le__(self,other):
        """ This method returns true if other >= self."""
        return other >= self


