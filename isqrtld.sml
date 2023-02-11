fun isqrtld (xs: string): string * string = 
    let
        fun divide_in_pairs(xs : string) : string list =
            let
                val xs = if String.size xs mod 2 = 1 then "0" ^ xs else xs
                val n = String.size xs
                fun loop (i : int) : string list =
                if i < n then
                    let val s = String.substring (xs, i, 2)
                    in (loop (i + 2))@[s] end
                else []
            in loop (0) end

        fun reverse(s: string) : string =
            let
                val n = String.size s
                fun loop(i: int) : string =
                    if i < n then
                        let val c = String.substring(s, i, 1)
                        in c ^ (loop (i + 1)) end
                    else ""
            in loop (0) end
        
        fun multSt(s: string, x: int): string =
            let
                val n = String.size s
                fun loop(i: int, carry: int, result: string): string =
                if i < 0 then
                    if carry = 0 then
                    result
                    else
                    Int.toString(carry) ^ result
                else
                    let
                    val digit = String.substring(s, i, 1)
                    val product = valOf(Int.fromString(digit)) * x + carry
                    val new_carry = product div 10
                    val new_digit = product mod 10
                    val new_result = Int.toString(new_digit) ^ result
                    in
                    loop(i-1, new_carry, new_result)
                    end
            in
                loop(n-1, 0, "")
            end

        fun subSt(s1: string, s2: string): string =
            let
                val n = String.size s1
                val n2 = String.size s2
                fun reverse s = implode (foldl op:: [] (explode s));
                val rev_s1 = reverse s1
                val rev_s2 = reverse s2
                fun loop(i: int, carry: int, result: string): string =
                if i = n then
                    result
                else
                    let
                    val digit1 = String.substring(rev_s1, i, 1)
                    val digit2 = if i < n2 then String.substring(rev_s2, i, 1) else "0"
                    val diff = valOf(Int.fromString(digit1)) - valOf(Int.fromString(digit2)) - carry
                    val new_carry = if diff < 0 then 1 else 0
                    val new_digit = if diff < 0 then diff + 10 else diff
                    val new_result = Int.toString(new_digit) ^ result
                    in
                    loop(i+1, new_carry, new_result)
                    end
                in
                loop(0, 0, "")
            end

        fun compare(s1: string, s2: string): int = 
            let
                val n = String.size s1
                val n2 = String.size s2
                fun add_zero(s: string, n: int): string =
                if String.size s >= n then
                    s
                else
                    add_zero("0" ^ s, n)
                val s1 = add_zero(s1, n2)
                val s2 = add_zero(s2, n)
                val n = String.size s1
                fun loop(i: int): int = 
                if i = n then
                    0
                else
                    let
                    val digit1 = valOf(Int.fromString(String.substring(s1, i, 1)))
                    val digit2 = valOf(Int.fromString(String.substring(s2, i, 1)))
                    in
                    if digit1 < digit2 then
                        ~1
                    else if digit1 > digit2 then
                        1
                    else
                        loop(i+1)
                    end
            in
                loop(0)
            end

        fun nextDigit (dividend: string, divisor: string) : int =
            let
            
            fun loop(i: int): int =
                if i = 10 then
                9
                else if compare(multSt(divisor ^ Int.toString(i), i), dividend) > 0 then
                i-1
                else
                loop(i+1)
            in loop 0 end

        val x_list = divide_in_pairs xs
        val n = List.length x_list
        
        fun recr(idx, x_list): string list = 
            if idx > 0 then
                let
                val value = hd x_list
                val last_rec = recr (idx-1, tl x_list)
                val last_quotient = hd last_rec
                val last_remainder = hd (tl last_rec)
                val curr_dividend = last_remainder ^ value
                val curr_divisor = multSt(last_quotient, 2)
                val new_digit = nextDigit (curr_dividend, curr_divisor)
                val curr_divisor = curr_divisor ^ Int.toString(new_digit)
                val curr_remainder = subSt(curr_dividend, multSt(curr_divisor, new_digit))
                val curr_quotient = last_quotient ^ Int.toString(new_digit)
                in
                [curr_quotient, curr_remainder]
                end
            else
                ["0","0"]
        val result = recr(n, x_list)
        val a = hd result
        val b = hd (tl result)
        fun remove_zero(s: string): string = 
            if s = "0" then
                s
            else if String.substring(s, 0, 1) = "0" then
                remove_zero(String.substring(s, 1, String.size s - 1))
            else
                s
        val a = remove_zero(a)
        val b = remove_zero(b)
    in 
        (a, b)
    end;;

isqrtld "3578210349689";