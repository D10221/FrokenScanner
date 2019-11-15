let queue (input: string)= 
    let mutable index = 0
    let get i = input.[i]    
    let peek i =  (index + i)|> get 
    let fwd i =
        index <- index + i
        get(index)        
    (peek, fwd , get)