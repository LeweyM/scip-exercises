					; 2.63

					; 1. both procedures produce the same list. tree->list-1 appends the list of the left branch to the entry of the node to the list of the right branch. tree->list-2 appends the entry of the left-most node to the list of the right branch. Both of these processes, when applied recursivly, produce identical lists of the trees from left to right.

					; 2. tree->list-1 recurses on both branches, meaning that each sub-tree will have to be computed 'from scratch' by recursivly computing their sub-trees. tree->list-2 only recurses on the right branch at the end of the opperation. This will therefore only visit each node once and so has n(o) complexity.

					; 2.64

					; 1. The procedure partial-tree works by dividing any ordered list into three parts. For example, the list (1 2 3) would be divided into 1, 2, and 3. 1 and 3 would be called recursively with the partial-tree procedure - the resulting trees would become the left and right trees and '2 would be become the entry for the node. As each node will have (almost) equal number of nodes in the left and right side, the tree will be balanced.

					;Example:
(1 3 5 7 9 11)

					; becomes

(5 (3 (1 '() '())
      '())
   (9 (7 '() '())
      (11 '() '())))

					; 2. The big o complexity of partial-tree procedure is linear to n because the algorithm passes though each node once and the work being done at each node is constant.

					;2.65

					; The trick is to use the fast conversions between lists and trees inorder to quickly and simply perform the union and intersection operations. The operations tree->list-2 o(n), union-set-ordered o(n), and list->tree o(n) all scale linearly with n.

(define (union-set-tree t1 t2)
  (list->tree (union-set-ordered (tree->list set1) (tree->list set2))))

(define (intersection-set-tree t1 t2)
  (list->tree (intersection-set-ordered (tree->list set1) (tree->list set2)))) 

  
