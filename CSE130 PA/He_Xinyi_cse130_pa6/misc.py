import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

# closest_to: Loop through the list l to find out the element which is closest to v.
# If l is expty, then return none. 
def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    if len(l) == 0:
      return None
    else:
      l1 = [(abs(d-v),d) for d in l] 
      l1 = sorted(l1)
      return l1[0][1]


# make_dict: Takes two list key and value, then return a dictionary pairing corresponding keys to values.
def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    dict1 = {}
    for i in range(len(keys)):
      dict1[keys[i]] = values[i]
    return dict1
   

# file IO functions
# word_count: open the file and return the count of each word.
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
    file = open(fn)
    text = file.read()
    #split by non alphanumeric and underscore characters
    data = re.split("[^\\w]+",text.lower())
    count = {}
    for word in data:
      if word == '':
        continue
      if word not in count:
        count[word] = 0
      count[word] += 1
    return count








