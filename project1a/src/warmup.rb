def fib(n)
    a = Array.new;
    for i in 0..n-1
        if i == 0 || i == 1
            a.append(i);
        else
            a.append(a[i-1] + a[i-2]);
        end
    end
    a
end

def isPalindrome(n)
    string = n.to_s
    len = string.length

    for i in 0...(len/2)
        if string[i] != string[len-i-1]
            return false
        end
    end
    true     
end

def nthmax(n, a)
    if a.length-n-1 < 0
        return nil
    else
        a.sort!;
        a[a.length-n-1]
    end
end

def freq(s)
    if s.length == 0
        return ""
    else
        arr = s.split("");
        char = arr[0];
        cnt = 1;
        max = 0;
        max_char = arr[0];
        for i in 1...arr.length;
            if arr[i] == char
                cnt = cnt + 1;
                if cnt > max
                    max = cnt;
                    max_char = arr[i];
                end
            else
                char = arr[i];
                cnt = 1;
            end
        end
        return max_char
    end
end

def zipHash(arr1, arr2)
    if arr1.length != arr2.length
        return nil
    else
        hash = Hash.new;
        for i in 0...arr1.length
            hash[arr1[i]] = arr2[i];
        end
        hash
    end
end

def hashToArray(hash)
    if hash.empty? == 0
        return []
    else

        arr = Array.new;
        keys = hash.keys;
        for i in 0...keys.length
            if keys[i] != nil && hash[keys[i]] != nil
                new_arr = Array.new;
                new_arr.push(keys[i]);    # maybe have to make these deep copies
                new_arr.push(hash[keys[i]]);
                arr.push(new_arr);
            end
        end
        arr
    end
end
