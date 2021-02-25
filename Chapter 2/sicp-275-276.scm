					; 2.75

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos a)))
          ((eq? op 'imag-part) (* mag (sin a)))
          ((eq? op 'magnitude)
           mag)
          ((eq? op 'angle) ang)
          (else
           (error "Unknown op: 
            MAKE-FROM-MAG-ANG" op))))
  dispatch)
				       
					; 2.76

#| 

Direct Dispatch: 

Generic methods are invoked by selecting the type directly where the method is called.

Adding new types: Every invocation must account for the new type.

Adding new operations: Every type must implement the new operation.


Data Directed: 

Generic methods use a look up table to find the appropriate type.

Adding new types: As the lookup table decouples the invocation from the set of types, you only need to add the type to the table with its operations.

Adding new operations: As we do not know which type will be used, every type must account for all operations. When adding a new operation, all types must be modified to support it.

** However: The decoupling could be implemented so that operations are stored in the lookup table. This will reverse the dependencies, so that adding a new type means that all operations must be able to support the new type, but adding a new operation only requires adding to the lookup table.


Message passing:

Generic methods are called on data objects which are bundled with their operations which know how to handle certain messages.

Adding new types: This is simply adding a new data object which can handle the necessary operations.

Adding new operations: This requires modifying all types to be able to handle the new message (operation).


Which and when:

In all cases, adding a new operation requires modifying the types and adding a type requires adding the operations. The difference is how localized the changes are. 

Message passing and Data Directed method with a lookup table for types both couple the types to their operations, so that when a new type is created all of the changes in the code necessary can be performed in the same place. This would be best for a system in which adding many types would be useful.

A data directed method where the lookup table uses operators as a key, however, means that operators are bound together in the same place. Adding extra operators in this case is a matter of adding an operator for every type to the lookup table. This would be best for a system in which adding many operators would be useful.



#|
