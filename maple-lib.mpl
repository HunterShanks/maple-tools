SymbolicComputationTools := module()
description "The SymbolicComputationTools package is a collection of procedures to work with common algorithms seen in symbolic computation";
option package;
export euclid, euclidExt, ring_zero_div, ring_units, ring_comple_zero_div, mod_mult_inv, square_base_mod;

    euclid := proc(a::integer, b::integer)
    description "Computes GCD of 2 integers a and b using the euclidean algorithm";
        # End condition, once a = 0, we have reached the GCD [this is when the remainder is 0]
        if a = 0 then
            return b;
        end if;
        # Recursively call the function, but passing b mod a
        return euclid(b mod a, a);
    end proc:

    euclidExt := proc(a::integer, b::integer)
    description "Computes GCD of 2 integers a and b using the extended euclidean algorithm"
    local gcd::integer, row_minus_1::integer, row_minus_2::integer, x::integer, y::integer; 
        # End condition, once a = 0, we have reached the GCD [this is when the remainder is 0]
        if a = 0 then
            return b,0,1;
        end if;
        # Recursively call the function but passing b mod a
        gcd, row_minus_1, row_minus_2 := euclidExt((b mod a), a);
        # Here x is the current row
        # row_minus_2 is the current row - 2
        # row_minus_1 is the current row - 1
        # Update x and y using the results of the recursive call
        x := row_minus_2 - iquo(b, a) * row_minus_1;
        y := row_minus_1;
        # Print out matrix row by row
        print(b, row_minus_1, row_minus_2);
        return gcd,x,y;
    end proc:

    ring_zero_div := proc(m::integer)
    description "Computes zero divisors of a ring m"
    local curr_val::integer;
        # Loop through each element of the set
        for curr_val to m do
            # Checks zero divisor condition (checks if 1 < gcd(current_value, m) < m)
            if gcd(curr_val, m) > 1 and gcd(curr_val, m) < m then
                print(curr_val);
            end if;
        end do;
    end proc:

    ring_units := proc(m::integer)
    description "Computes units of a ring m"
    local curr_val::integer;
        # Loop through each element of the set
        for curr_val to m do
            # Checks unit condition (check for coprimes: gcd(current_value, m) = 1)
            if gcd(curr_val, m) = 1 then
                print(curr_val);
            end if;
        end do;
    end proc:

    ring_comple_zero_div := proc(m::integer, a::integer)
    description "Computes complementary zero divisors an element 'a' of a ring m"
    local curr_val::integer;
        # Loop through each element of the set
        for curr_val to m - 1 do
            # Checks complement zero divisor condition (check if (a * current_value) mod m = 0)
            if (a * curr_val) mod m = 0 then
                print(curr_val);
            end if;
        end do;
    end proc:

    mod_mult_inv := proc(a::integer, m::integer)
    description "Computes the modular multiplicative inverse of two integers a and m such that a mod m";
    local x::integer;
        # Loop through each element of the set
        for x to m do
            # Checks the modular multiplicative inverse
            if ((a mod m) * (x mod m)) mod m = 1 then
                return x;
            end if;
        end do;
    end proc:

    square_base_mod := proc(b::integer, exp::integer, m::integer)
    description "Computes the square base mod based on the lecture 6 notes";
    local bin::integer, num_bin::integer, a::integer, factors::integer, k::integer, j::integer, ans::integer;
        # Use 'base' instead of 'binary' to get a list of binary digits rather than just the binary number
        bin := convert(exp, base, 2);
        # Number of binary digits in the list
        num_bin := nops(bin);
        a := b mod m;
        factors := [a];
        # Loop through each element to create a list of square factors
        for k from 2 to num_bin do
            a := a^2 mod m:
            factors := [op(factors), a];
        end do;
        # Loop through each element and if they are not a zero in the list, multiply the elements.
        for j to num_bin do
            if bin[j] > 0 then
                ans := ans * factors[j] mod m;
            end if;
        end do;
        return ans;
    end proc:

    eulerPhi := proc(m::integer)
    description "Computation of the Euler Phi/ Euler Totient alogrithm to retrieve the number of integers relatively prime to the integer m";
    local i::integer, phi::integer;
        phi := 0;
        for i from 1 to m do
            if gcd(m,i) = 1 then
                phi := phi + 1;
            end if;
        end do;
        return phi;
    end proc: