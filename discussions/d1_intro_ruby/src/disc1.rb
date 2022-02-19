# We will be implimenting a simple database table using Ruby data structures to store the data.
# The class Tuple represents an entry in a table.
# The class Table represents a collection of tuples.

class Tuple
    @@numTuples = Hash.new(0) # if 0 is specified default value to return

    # data is an array of values for the tuple
    def initialize(data)
        @tuple = data;
        @@numTuples[data.length] += 1;
    end

    # This method returns the data at a particular index of a tuple (0 indexing)
    # If the provided index exceeds the largest index in the tuple, nil should be returned.
    # index is an Integer representing a valid index in the tuple.
    def getData(index)
        @tuple[index]
    end

    def getLength()
        @tuple.length
    end
    # This method should return the number of tuples of size n that have ever been created
    # hint: you should use a static variable
    # hint2: a hash can be helpful (though not strictly necessary!)
    def self.getNumTuples(n) 
        @@numTuples[n]
    end
end

class Table
    # column_names is an Array of Strings
    def initialize(column_names)
        @tuples = []; #semicolons to not return
        @colIndex = {};
        @len = column_names.length;
        for i in 0...column_names.length do
            @colIndex[column_names[i]] = i;
        end

    end

    # This method inserts a tuple into the table.
    # Note that tuples inserted into the table must have the right number of entries
    # I.e., the tuple should be the size of column_names
    # If the tuple is the correct size, insert it and return true
    # otherwise, DO NOT insert the tuple and return false instead.
    # tuple is an instance of class Tuple declared above.
    def insertTuple(tuple)
        if len != tuple.getLength
            return false # return can be used to end code/exit
        end
        @tuples.push(tuple);
        true
    end
    
    # Given a column name and a value, this method finds the number of rows where the value 
    # for the column matches the given value.
    def numRowsWhere(column,value)
        numRows = 0;
        for i in 0...@tuples.length do
            if @tuple[i].getData(colIndex[column]) == value
                numRows += 1;
            end
        end
        numRows
    end 

end
