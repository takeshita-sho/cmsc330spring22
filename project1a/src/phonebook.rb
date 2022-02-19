class PhoneBook
    def initialize
        @book = Hash.new
    end

    def add(name, number, is_listed)
        if @book.has_key?(name) || !(number =~ /\d{3}-\d{3}-\d{4}$/) || (is_listed == true && @book.has_value?([number, true])) #prolly have to change
            return false            
        else
            arr = [number, is_listed];
            @book[name] = arr;
            return true
        end
    end

    def lookup(name)
        if @book.has_key?(name)
            val = @book[name];
        else
            return nil
        end
        if val[1]
            val[0]
        else
            nil
        end
    end

    def lookupByNum(number)
        if @book.has_value?([number, true])
            @book.key([number, true])
        else
            nil
        end
    end

    def namesByAc(areacode)
        names = Array.new;
        @book.each do |name, value|
            if value[0] =~ /^#{areacode}/
                names.push(name);
            end
        end
        names
    end
end
