
from turtle import right
from linked_list import LinkedList


class BST:

    class Node:
        # Initialize a node with a key, left, and right child nodes
        def __init__(self, key, left=None, right=None):
            self.key = key
            self.left = left
            self.right = right

        def __iter__(self):     # Discussed in the text on generators

             # Check if there is a left child node. If there is, it means there are smaller keys in the left subtree.
            if self.left:
             # Yields all the keys in the left subtree in sorted order because smaller keys are always stored on the left side of a BST
                yield from self.left
             # Yields the key of current node
            yield self.key
            if self.right:
                yield from self.right

    def __init__(self, root=None):
        self.root = root

    def __iter__(self):         # Dicussed in the text on generators
        if self.root:
            yield from self.root

    def insert(self, key):
        self.root = self._insert(self.root, key)

    def _insert(self, r, key):
        if r is None:
            return self.Node(key)
        elif key < r.key:
            r.left = self._insert(r.left, key)
        elif key > r.key:
            r.right = self._insert(r.right, key)
        else:
            pass  # Already there
        return r

    def print(self):
        self._print(self.root)

    def _print(self, r):
        if r:
            self._print(r.left)
            print(r.key, end=' ')
            self._print(r.right)

    def contains(self, k):
        n = self.root
        while n and n.key != k:
            if k < n.key:
                n = n.left
            else:
                n = n.right
        return n is not None

    def size(self):
        return self._size(self.root)

    def _size(self, r):
        if r is None:
            return 0
        else:
            return 1 + self._size(r.left) + self._size(r.right)
        



#   Methods to be completed


    def height(self):  # Compulsory
        def _height(node):
            if node is None:
                return 0
            left_height = _height(node.left)  
            right_height = _height(node.right) 
            return max(left_height, right_height) + 1

        return _height(self.root) 
        


    def remove(self, key):
        self.root = self._remove(self.root, key)

    def _remove(self, r, k):
        def smallest(root):
            if root is None:
                return None

            current = root

            while current.left is not None:
                current = current.left

            return current.key

        if r is None:
            return None

        elif k < r.key:
            r.left = self._remove(r.left, k)

        elif k > r.key:
            r.right = self._remove(r.right, k)

        else:  # Now find where r == k
            if r.left is None:
                return r.right
            elif r.right is None:
                return r.left
            else:  # if the key has two children
                min_in_right = smallest(r.right)
                r.key = min_in_right
                r.right = self._remove(r.right, min_in_right)

        return r


            
    def __str__(self):                            # Compulsory
        def _str(node):
            if node is None:
                return ""
            
            if node.left:
                yield from _str(node.left)

            yield str(node.key)

            if node.right:
                yield from _str(node.right)
            

        result = list(_str(self.root))
        result = "<" + ", ".join(result) + ">"
        return result


#The timecomplexity of the code is O(n)
    # def to_list(self):                           #Compulsory
    #     def lst(node):
    #         if node is None:
    #             return []
            
    #         yield from lst(node.left)
    #         yield from lst(node.right)
    #         yield node.key

    #     return list(lst(self.root))

    def to_list(self):
        def lst(node):
            if node is None:
                return []

            result = []
            result.extend(lst(node.left))
            result.append(node.key)
            result.extend(lst(node.right))
            return result

        return lst(self.root)


    def to_LinkedList(self):
        def generator(node):

            if node is None:
                return LinkedList()
            
            if node:
                yield from generator(node.left)
                yield node.key
                yield from generator(node.right)
                

        linked_list = LinkedList()

        for value in generator(self.root):
            linked_list.insert(value)

        return linked_list



    def ipl(self):
        def _ipl(node, depth):
            if node is None:
                return 0
            return depth + _ipl(node.left, depth + 1) + _ipl(node.right, depth + 1)

        return _ipl(self.root, 1)




#each node has a "root" value and a list of "children" nodes.
import random

def random_tree(n):
    def generate_tree(node_count):#node_counts:the number of nodescurrent call should create
        if node_count <= 0:
            return None
        if node_count == 1:
            return {"root": random.random(), "children": []}

        root_value = random.random()
        left_child_count = random.randint(0, node_count - 1) #Determin the total number of left children
        right_child_count = node_count - 1 - left_child_count#calculate the total number of right children

        return {
            "root": root_value,
            "children": [
                generate_tree(left_child_count),
                generate_tree(right_child_count)
            ]
        }

    return generate_tree(n)


def _height(tree):
    if tree is None:
        return 0

    max_child_height = 0
    for child in tree.get("children", []):
        child_height = _height(child)
        max_child_height = max(max_child_height, child_height)

    return max_child_height + 1
 

def _ipl(tree):
    def helper(node, level):
        if node is None:
            return 0
        
        total_depth = level

        for child in node.get("children", []):
            total_depth += helper(child, level + 1)

        return total_depth

    return helper(tree, 1)


def main():
    import math
    tree_sizes = [1000,2000]

    print(f"{'n'}  {'Theory'}  {'IPL/n'}  {'height'}")

    for item in tree_sizes:
        tree = random_tree(item)
        n = item
        theory = 1.39 * math.log2(n)
        observed = _ipl(tree) / item
        tree_height = _height(tree)

        print(f"{n}  {theory}  {observed}  {tree_height}")


if __name__ == "__main__":
    main()


"""
What is the generator good for?
==============================

1. computing size? Yes, the generator will make this process faster
2. computing height? No
3. contains? No
4. insert? Yes
5. remove? NO, the genrator will make this process sLower




Results for ipl of random trees
===============================
when tree size is 1000, the ipl is 12.059
when tree size is 2000, the ipl is 13.61





"""
