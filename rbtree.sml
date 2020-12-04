signature HASHMAP = 
sig
  type key
  type 'a t

  val empty  : 'a t
  val insert : 'a t -> (key * 'a) -> 'a t
  val lookup : 'a t -> key -> 'a option

  val map    : 'a t -> ('a -> 'b) -> 'b t
  val size   : 'a t -> int
  val values : 'a t -> 'a list
end


functor RedBlackTree(
  type key;
  val compare: key * key -> order
) : HASHMAP = 
struct 
  type key = key;
  type 'a entry = key * 'a
  datatype 'a t 
    = E 
    | R of 'a entry * 'a t * 'a t 
    | B of 'a entry * 'a t * 'a t

  val empty = E

  fun balance (B (z, R(y, R(x, a, b), c), d)) = R(y, B(x,a,b), B(z, c, d))
    | balance (B (z, R(x, a, R(y, b, c)), d)) = R(y, B(x,a,b), B(z, c, d))
    | balance (B (x, a, R(z, R(y, b, c), d))) = R(y, B(x,a,b), B(z, c, d))
    | balance (B (x, a, R(y, b, R(z, c, d)))) = R(y, B(x,a,b), B(z, c, d))
    | balance x = x

  fun insert tree (key, v) = 
    let         
      fun insl C ((key', data), a, b) = 
        case compare (key, key')
            of LESS    => balance (C((key', data), ins a, b))
             | EQUAL   => C((key', data), a, b)
             | GREATER => balance (C((key', data), a, ins b))
      and ins E = R((key, v), E, E)
        | ins (R tree') = insl R tree'         
        | ins (B tree') = insl B tree'
    in
      (* recolor root to black *)
      case ins tree
        of R(y, a, b) => B(y, a, b)
         | x => x
    end
  
  fun lookup (B tree) k = lk tree k
    | lookup (R tree) k = lk tree k 
    | lookup E        _ = NONE
  and lk ((k',v), a, b) k = 
    case compare (k, k')
      of EQUAL   => SOME v
       | LESS    => lookup a k
       | GREATER => lookup b k

  fun map (B ((k, v), a, b)) f = B((k, f v), map a f, map b f)
    | map (R ((k, v), a, b)) f = R((k, f v), map a f, map b f)
    | map E _ = E 
  
  fun size E = 0
    | size (B(_, l, r)) = 1 + size l + size r
    | size (R(_, l, r)) = 1 + size l + size r

  fun values E = []
    | values (B((k, v), a, b)) = values a @ [v] @ values b
    | values (R((k, v), a, b)) = values a @ [v] @ values b

end 
