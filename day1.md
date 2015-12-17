# Day 1

## Part 1

```Ocaml
let explode s = 
let rec explode_char_at i l = match i with
| i when i < 0 -> l
| i -> explode_char_at (i-1) ((String.get s i)::l) in
explode_char_at ((String.length s) - 1) [];;

let floor s = 
let rec floor_list l = match l with
| [] -> 0
| x :: xs -> if x == '(' then (floor_list xs) + 1 else (floor_list xs) - 1 in
floor_list (explode s);;
```

### Alternative (smaller, all in one)
```Ocaml
let new_floor s = 
let rec floor_helper acc i s max = match i with
| i when i == max -> acc
| i -> begin 
let c = String.get s i in match c with
| '(' -> floor_helper (acc+1) (i+1) s max
| _ -> floor_helper (acc-1) (i+1) s max
end in
floor_helper 0 0 s (String.length s);;
```

## Part 2 (including previous code)
```Ocaml
let basement s =
let rec basement_list l acc count = match l with
| [] -> if acc == -1 then Some count else None
| x::xs -> if acc == -1 then Some count else if x == '(' then basement_list xs (acc+1) (count+1) else basement_list xs (acc-1) (count+1) in
basement_list (explode s) 0 0;;
```
