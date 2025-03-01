
"""
Important notes: 
These examples are intended to practice RECURSIVE thinking. Thus, you may NOT 
use any loops nor built in functions like count, reverse, zip, math.pow etc. 

You may NOT use any global variables.

You can write code in the main function that demonstrates your solutions.
If you have testcode running at the top level (i.e. outside the main function)
you have to remove it before uploading your code into Studium!
Also remove all trace and debugging printouts!

You may not import any packages other than time and math and these may
only be used in the analysis of the fib functionen.

In the oral presentation you must be prepared to explain your code and make minor 
modifications.

We have used type hints in the code below (see 
https://docs.python.org/3/library/typing.html).
Type hints serve as documatation and and doesn't affect the execution at all. 
If your Python doesn't allow type hints you should update to a more modern version!

"""




from email.mime import base
import time
import math


def power(x,n):                           # Optional
    if n == 0:
        return 1
    elif n < 0:
        return 1 / power(x, -n)
    else:
        return x * power(x,n-1)
    pass


def multiply(m: int, n: int) -> int:         # Compulsory
    if m == 0 or n == 0:
            return 0
     
    return n + multiply(m-1,n)
    pass

# Minimize the number of recursive calls:
# def multiply_1(m,n):
#     if m == 0 or n == 0:
#             return 0
#     if m < n:
#         return n + multiply(m-1,n)
#     else: 
#         return m + multiply(m,n-1)




def divide(t: int, n: int) -> int:           # Optional
    if t < n:
        return 0
    
    return 1 + divide(t-n,n)
    pass


def harmonic(n: int) -> float:                 # Compulsory
    if n == 1:
        return 1
    return harmonic(n-1) + 1/(n)
    pass



def get_binary(x: int) -> str:               # Compulsary
    
    bbinary_num = ''
    
    if x == 0:
        return '0'
    elif x == 1:
        return '1'
    
    elif x < 0:
        return '-' + get_binary(-x)
    
    else: 
        binary_num = get_binary(x // 2)
        return binary_num + str(x % 2)
    pass


def reverse_string(s: str) -> str:           # Optional
    if len(s) <= 1:
        return s
    else:
        return reverse_string(s[1:]) + s[0]
    pass


def largest(a: iter):                        # Compulsory
    maximum = a[0]
    
    if len(a) == 1:
        return a[0]
    
    if maximum > largest(a[1:]):
        return maximum
    else:
        return largest(a[1:])
    pass


def count_in_highest_level(x, s: list) -> int:                # Compulsory
    counts = 0
    
    if s == []: 
        return 0
    
    if s[0] == x:
        counts += 1
        
    counts = counts + count_in_highest_level(x,s[1:])
    return counts

#Count at all level:
def count(x,s):

    counts = 0
    
    if s == []:
        return 0
    if s[0] == x:
        counts += 1
    if type(s[0]) == list:
        counts += count(x, s[0])
    
    counts = counts + count(x,s[1:])
    
    return counts

    pass


def zippa(l1: list, l2: list) -> list:       # Compulsory

    if l1 == [] and l2 == []:
        return []
    if l1 == []:
        return l2
    if l2 == []:
        return l1
    
    else:
        return [l1[0], l2[0]] + zippa(l1[1:], l2[1:])
    pass


def bricklek(f: str, t: str, h: str, n: int) -> str:  # Compulsory    
    if n == 0:       
        return []    
    else:          
        return bricklek(f, h, t, n-1) + [f + '->' + t] + bricklek(h, t, f, n-1)    
    pass



def fib(n: int) -> int:                       # Compulsory
    """ Returns the n:th Fibonacci number """
    # You should verify that the time for this function grows approximately as
    # Theta(1.618^n) and also estimate how long time the call fib(100) would take.
    # The time estimate for fib(100) should be in reasonable units (most certainly
    # years) and, since it is just an estimate, with no more than two digits precision.
    #
    # Put your code at the end of the main function below!
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n-1) + fib(n-2)


def main():
    print('\nCode that demonstates my implementations\n')

    print('\n\nCode for analysing fib\n')

    print('\nBye!')


if __name__ == "__main__":
    main()

##Verification of time complexity Theta(1.618^n)

#time to process fib(30):
import time
start = time.perf_counter()
fib(30)
stop = time.perf_counter()
print(f"The Measurement takes {stop-start} seconds")
#output: The Measurement takes 0.16823837500123773 seconds

#time to process fib(31):
import time
start = time.perf_counter()
fib(31)
stop = time.perf_counter()
print(f"The Measurement takes {stop-start} seconds")
#output: The Measurement takes 0.2641228749998845 seconds

#Culculate the time ratio between f(31)and f(30)
#0.2641228749998845 / 0.16823837500123773 = 1.60

#time to process fib(40):
import time
start = time.perf_counter()
fib(40)
stop = time.perf_counter()
print(f"The Measurement takes {stop-start} seconds")
#output:The Measurement takes 18.97646608299692 seconds

#Culculate the time ratio between f(40)and f(30)
#18.97646608299692//0.16823837500123773 = 112 = 1.618 ^ 10

##Thus, we can verify that Theta(1.618^n) is the time complexity od fibonacci function 


## Estimation of fib(100) from fib(40):
# t = 1.618 ** (100-40) * 18.97646608299692 = 65603404106592.77sec == 2080270 years







#Answers to the none-coding tasks
#================================
  
  


  #Exercise 16: Time for bricklek with 50 bricks:
  
# The total move in Hanoi tower is 2^n-1
# If take one second to one tile
# The entire transfer for a stack of 50 tile would take 2^50-1 seconds, approximately 35702051 years
  
  


  


#Exercise 17: Time for Fibonacci:


##Verification of time complexity Theta(1.618^n)
##Verification of time complexity Theta(1.618^n)

# #time to process fib(30):
# import time
# start = time.perf_counter()
# fib(30)
# stop = time.perf_counter()
# print(f"The Measurement takes {stop-start} seconds")
# #output: The Measurement takes 0.16823837500123773 seconds

# #time to process fib(31):
# import time
# start = time.perf_counter()
# fib(31)
# stop = time.perf_counter()
# print(f"The Measurement takes {stop-start} seconds")
# #output: The Measurement takes 0.2641228749998845 seconds

# #Culculate the time ratio between f(31)and f(30)
# #0.2641228749998845 / 0.16823837500123773 = 1.60

# #time to process fib(40):
# import time
# start = time.perf_counter()
# fib(40)
# stop = time.perf_counter()
# print(f"The Measurement takes {stop-start} seconds")
# #output:The Measurement takes 18.97646608299692 seconds

# #Culculate the time ratio between f(40)and f(30)
# #18.97646608299692//0.16823837500123773 = 112 = 1.618 ^ 10

# ##Thus, we can verify that Theta(1.618^n) is the time complexity od fibonacci function 


# ## Estimation of fib(100) from fib(40):
# # t = 1.618 ** (100-40) * 18.97646608299692 = 65603404106592.77sec == 2080270 years



  
# Exercise 20: Comparison sorting methods:
# Simple insertion sort:
#     The time complexity of insertion sort is O(n^2).
#     If the time for 1000 nums is 1 sec:
#         O(1000^2) = C*1000^2 = 1
#     Thus:
#         C = 10^-6
#     If the number of random numbers is 10^6,the sortiing time would be:
#         O((10^6)^2) = C * 10^12 = 10^6 seconds
#         Also 11.57 days
#     If the number of random numbers is 10^9, the sorting time would be:
#         O((10^9)^2) = C * 10^18 = 10^12 seconds
#         Also 31709 years

# Merge sort:
#     The time complexity of merge sort is O(n*logn)
#     If the time for 1000 nums is 1 sec:
#         O(1000*log10^3)
#     Thus:
#         C = 1/3000
#     If the number of random numbers is 10^6,the sortiing time would be:
#         O(10^6*log10^6)=C * 10^6*log10^6 = 2000 seconds
#         Also 33 minites
#     If the number of random numbers is 10^9, the sorting time would be:
#         O(10^9*log10^9)=C * 10^9*log10^9 = 3*10^6 seconds
#         Also 34.7 days
  
  
  
  



#Exercise 21: Comparison Theta(n) and Theta(n log n)
# O(n) = c*n*log(n)
# o(n) = n
# When O(n)<O(c*n*logn),algorithm A take less time than Algorithm B, that is:
#     n < c*n*log(n)
#     1 < c * log(n)
# For C:
#     c * (10 * log(10)) = 1 
#     c = 1 / (10 * log10)
# Thus only when n>10^10,algorithm A to take less time than algorithm B
  
  
  
  
  

