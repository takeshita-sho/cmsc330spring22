require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    board = GameBoard.new(10,10)

    if !read_file_lines(path) { |line|
        if board.ships != 5 && (line =~ /^([\(]){1}([1-9]|10){1}([,]){1}([1-9]|10){1}([\)]){1}([,]){1}(\s){1}(Right|Left|Up|Down){1}([,]){1}(\s){1}([1-5]){1}$/)
            arr = line.scan(/^([\(]){1}([1-9]|10){1}([,]){1}([1-9]|10){1}([\)]){1}([,]){1}(\s){1}(Right|Left|Up|Down){1}([,]){1}(\s){1}([1-5]){1}$/)
            shp = Ship.new(Position.new(arr[0][1].to_i, arr[0][3].to_i), arr[0][7], arr[0][10].to_i)
            board.add_ship(shp)
            
        end
    }
        return nil
    end
    if board.ships != 5
        return nil
    else
        return board
    end
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    arr = Array.new

    if !read_file_lines(path) { |line| 
        if line =~ /^(\(){1}(-?\d+)(,){1}(-?\d+)(\)){1}$/
            pos = line.scan(/^(\(){1}(\d+)(,){1}(\d+)(\)){1}$/)
            arr.append(Position.new(pos[0][1].to_i, pos[0][3].to_i))
        end
    }
        return nil
    end
    return arr
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
