

from unittest.mock import NonCallableMagicMock


class LinkedList:

    class Node:
        def __init__(self, data, succ):
            self.data = data
            self.succ = succ

    # Initializes an empty linked list with self.first set to None.
    def __init__(self):
        self.first = None


    # __iter__(self): GENERATOR. Allows to iterate through the elements of the linked list. 
    # It yields the data of each node in the list.
    # Discussed in the section on iterators and generators
    def __iter__(self):            
        current = self.first
        while current:
            yield current.data
            current = current.succ

 
    #Overloads the IN operator (in keyword) to check if a given value x is present in the linked list. 
    #It iterates through the list and returns True if x is found, or False otherwise.
    # Discussed in the section on operator overloading
    def __in__(self, x):           
        for d in self:
            if d == x:
                return True
            elif x < d:
                return False
        return False


   #Inserts a new node with data x into the linked list while maintaining the list in sorted order. 
   #It does this by iterating through the list to find the appropriate position for the new node.
    def insert(self, x):
        if self.first is None or x <= self.first.data:
            self.first = self.Node(x, self.first)
        else:
            f = self.first
            while f.succ and x > f.succ.data:
                f = f.succ
            f.succ = self.Node(x, f.succ)
    
    #Prints the elements of the linked list enclosed in parentheses. 
    #It iterates through the list and prints each element followed by a comma and space if there are more elements.
    def print(self):
        print('(', end='')
        f = self.first
        while f:
            print(f.data, end='')
            f = f.succ
            if f:
                print(', ', end='')
        print(')')

    # To be implemented


    def length(self):            #Optional
        count = 0
        current = self.first

        while current:
            count += 1
            current = current.succ

        return count



    def mean(self):               # Optional
        if not self.first:
            return None
        
        counts = self.length()
       
        total = 0 
        current = self.first

        #Iterate through the linked list and process each node until there are no more nodes left
        while current:
            total += current.data
            current = current.succ

        return total / counts




    def remove_last(self):        #Optional
        if not self.first: #Check if is empty list
            raise ValueError("The list is empty")

        if not self.first.succ: #If only one node, afte removal, the list is empty
            result = self.first.data
            self.first = None
            return result

        current = self.first
        while current.succ and current.succ.succ:
            current = current.succ  #traverses the list until reach the second-to-last node

        result = current.succ.data
        current.succ = None  # Remove the last node from the list
        return result

        
        

    def remove(self, x):
        
        if self.first is None:
            return False

        current = self.first

        if current.data == x: #Check specifically if the x value appear at the first node: first node does not have previous node
            self.first = current.succ
            return True

        while current.succ:#Check from the second node
            if current.succ.data == x:
                current.succ = current.succ.succ
                return True
            current = current.succ

        return False




    def count(self, x):           # Optional
        if self.first is None:
            return 0
        
        current = self.first 
        counts = 0
        while current: 
            if current.data == x: 
                counts += 1
            current = current.succ
        
        return counts


    def to_list(self):
        def _to_list(current):
            if current is None:
                return []
            
            data_list = [current.data]
            
            if current.succ:
                data_list.extend(_to_list(current.succ))
            
            return data_list
    
        return _to_list(self.first)


        
        

    def remove_all(self, x):      # Compulsory

        def _remove_all(current):

            if current is None:
                return None

            if current.data == x:
                return _remove_all(current.succ) #continue removing x
            else:
                current.succ = _remove_all(current.succ)
                return current
    
        self.first = _remove_all(self.first)


   
    
    def __str__(self):            # Compulsary
        result = ''
        f = self.first
        while f:
            result += str(f.data)
            f = f.succ
            if f:
                result += ', '
        
        return'(' + result + ')'


# Complexity for this implementation: 

# The complexity of the original copy() is:O(n^2). 
# Because it iterate throughout ('for') the whole list for insertion, which give a complexity of O(n)
# And complexity of the insertion method itself is O(n)
# Thus, the time commplexity of the whole function is O(n^2)

    # def copy(self):               # Compulsary
    #     result = LinkedList()
    #     for x in self:
    #         result.insert(x)
    #     return result
    


# Complexity for revised implementation: O(n). 
# Because while loop traversed the entire original linked list, which gives the complexity O(n).
# No insert() method in the function
    def copy(self):
        result = LinkedList()

        current = self.first
        while current: #Iterate thoughout the original list
            new_node = self.Node(current.data, None)  # Copy current data to a new node
            
            #Add the very first node to the new list
            if not result.first: #Check if the new list is empty 
                result.first = new_node  # Set the first node of the new list
                result.last = new_node  # track of the last node in the linked list. MAke sure the 'last' attribute of the result linked list to point to the newly created new node

            #Add the following node to the new list
            else:
                result.last.succ = new_node  # updates the succ of the current last node to point to the new_node
                result.last = new_node  # Update the last node to the newly created node

            current = current.succ

        return result


        

    def __getitem__(self, ind):   # Compulsory

        current = self.first
        count = 0
        while current:
            if count == ind:
                return current.data
            count += 1
            current = current.succ
            
        raise IndexError("Index out of range")


class Person:
    class Node:
        def __init__(self, name, pnr):
            self.name = name
            self.pnr = pnr

        def __lt__(self, other):
            return self.pnr < other.pnr

        def __le__(self, other):
            return self.pnr <= other.pnr

        def __eq__(self, other):
            return self.pnr == other.pnr

    def __init__(self):
        self.first = None

    def __iter__(self):
        current = self.first
        while current:
            yield current
            current = current.succ

    def __contains__(self, x):
        for d in self:
            if d.pnr == x:
                return True
            elif x < d.pnr:
                return False
        return False

    def insert(self, name, pnr):
        new_node = self.Node(name, pnr)
        if self.first is None or new_node <= self.first:
            new_node.succ = self.first
            self.first = new_node
        else:
            f = self.first
            while f.succ and new_node > f.succ:
                f = f.succ
            new_node.succ = f.succ
            f.succ = new_node

    def print(self):
        result = []
        f = self.first
        while f:
            result.append((f.name, f.pnr))
            f = f.succ
        print(result)


def main():
    # Create an empty custom list plist in main
    plist = Person()

   
    p = ("AB", "3")
    plist.insert(*p)  # Unpack the tuple p
    q = ("CD", "1")
    plist.insert(*q) 
    m = ("EF","4")
    plist.insert(*m)


    plist.print()

if __name__ == '__main__':
    main()
