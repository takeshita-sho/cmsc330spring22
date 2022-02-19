class GameBoard
    # @max_row is an `Integer`
    # @max_column is an `Integer`
    attr_reader :max_row, :max_column, :ships

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @hits = 0 #could not be counting right
        @ships = 0
        @pieces = 0 #prob not counting right
        @game_board = Array.new(@max_row) {Array.new(@max_column) {hash = {"ship" => "-", "attack" => "-"}}}
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        if (ship.size < 1) || ship.size > 5 || @max_column <= 0 || @max_row <= 0
            return false
        end
        row = ship.start_position.row
        column = ship.start_position.column
        if row > @game_board.size || row < 1 || column > @game_board[0].size || column < 1
            return false
        end
        if ship.orientation == "Up"
            if row - ship.size < 0
                return false
            end
            for i in (row-ship.size)...(row)
                if @game_board[i][column-1]["ship"] == "B"
                    return false
                end
            end
            for j in (row-ship.size)...(row)
                
                @game_board[j][column-1]["ship"] = "B"
            end
            @ships += 1
            @pieces += ship.size
            return true
        end
        if ship.orientation == "Down"
            if row + ship.size > @game_board.size + 1
                return false
            end
            for i in (row-1)...(row-1+ship.size)
                if @game_board[i][column-1]["ship"] == "B"
                    return false
                end
            end
            for j in (row-1)...(row-1+ship.size)
                @game_board[j][column-1]["ship"] = "B"
            end
            @ships += 1
            @pieces += ship.size
            return true
        end
        if ship.orientation == "Left"
            if column - ship.size < 0
                return false
            end
            for i in (column-ship.size)...(column)
                if @game_board[row-1][i]["ship"] == "B"
                    return false
                end
            end
            for j in (column-ship.size)...(column)
                @game_board[row-1][j]["ship"] = "B"
            end
            @ships += 1
            @pieces += ship.size
            return true
        end
        if ship.orientation == "Right"
            if column + ship.size > @game_board[0].size + 1
                return false
            end
            for i in (column-1)...(column-1+ship.size)
                
                if @game_board[row-1][i]["ship"] == "B"
                    return false
                end
            end
            for j in (column-1)...(column-1+ship.size)
                @game_board[row-1][j]["ship"] = "B"
            end
            @ships += 1
            @pieces += ship.size
            return true
        end
        return false
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
        # check position
        if position.row > @game_board.size || position.row < 1 || position.column > @game_board[0].size || position.column < 1
            return nil
        end

        # update your grid
        if @game_board[position.row-1][position.column-1]["attack"] != "A" && @game_board[position.row-1][position.column-1]["ship"] == "B"
            @hits = @hits + 1
            
        end
        @game_board[position.row-1][position.column-1]["attack"] = "A"
        # return whether the attack was successful or not
        if @game_board[position.row-1][position.column-1]["ship"] == "B"
            return true
        else
            return false
        end
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        if @max_column <= 0 || @max_row <= 0
            return false
        end

        num = 0
        @game_board.each { |row|
            row.each { |col|
                if col["ship"] == "B"
                    if col["attack"] == "A"
                        num += 1
                    end
                end
            }
        } 
        return num
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        @game_board.each { |row|
            row.each { |col|
                if col["ship"] == "B"
                    if col["attack"] != "A"
                        return false
                    end
                end
            }
        } 
        return true
        #f @hits == @pieces
         #   return true
        #else
         #   return false
        #end
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
        str = ""
        for row in @game_board[0..]
            for col in row[0..]
                str << "| #{col["ship"]}, #{col["attack"]} |"
            end
            str << "\n"
        end
        str << "\n"
        return str
    end
end
