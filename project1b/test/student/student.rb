require "minitest/autorun"
require_relative "../../src/controllers/input_controller.rb"
require_relative "../../src/controllers/game_controller.rb"
require_relative "../../src/models/game_board.rb"
require_relative "../../src/models/position.rb"
require_relative "../../src/models/ship.rb"

# The ship coordinates for p1, p2
SHIPS_P1 = "/mnt/c/WINDOWS/system32/cmsc330spring22/project1b/test/public/inputs/correct_ships_p1.txt"
SHIPS_P2 = "/mnt/c/WINDOWS/system32/cmsc330spring22/project1b/test/public/inputs/correct_ships_p2.txt"

# The attack coordinates against p1, p2
ATTACK_P1 = "#{__dir__}/inputs/correct_strat_p1.txt"
ATTACK_P2 = "#{__dir__}/inputs/correct_strat_p2.txt"

# The perfect attack coordinates against p1, p2
PERF_ATK_P1 = "#{__dir__}/inputs/perfect_strat_p1.txt"
PERF_ATK_P2 = "#{__dir__}/inputs/perfect_strat_p2.txt"

# A bad ships file
BAD_SHIPS = "#{__dir__}/inputs/bad_ships.txt"

class PublicTests < MiniTest::Test
    def setup
        @p1_ships = []
        @p1_perf_atk = []
        @p2_ships = []
        @p2_perf_atk = []
        for i, size in [1,2,3,4].zip([4,5,3,2])
            pos0 = Position.new(i, i)
            pos1 = Position.new(i + 4, i + 4)
            @p1_ships << Ship.new(pos0, "Right", size)
            @p2_ships << Ship.new(pos1, "Right", size)
            for j in 0..(size - 1)
                @p2_perf_atk << Position.new(i, i + j)
                @p1_perf_atk << Position.new(i + 4, i + j + 4)
            end
        end
    end

    def test_public_gameboard_1
        puts read_ships_file(SHIPS_P2)
    end

    def test_public_gameboard_2
        # Property: (add a ship & attack the length of the ship) => no. of attacks on the ship == nm_successful_attacks
        test_board = GameBoard.new(10, 10)
        for shp in @p2_ships
            add_shp_ret = test_board.add_ship(shp)
            assert(add_shp_ret, "A valid ship was not added")
        end
        refute(test_board.all_sunk?, "There is atleast one ship standing, but board says they're sunk")
        for i in @p1_perf_atk
            refute(test_board.all_sunk?, "All the ships have not sunk but board thinks they have")
            assert(test_board.attack_pos(i), "Attack that should hit did not")
        end
        assert(test_board.all_sunk?, "All the ships have sunk but board thinks they have not")
        assert_equal(@p1_perf_atk.length, test_board.num_successful_attacks, "The successful attacks must be the same as the number of ship slots")
    end

    def test_public_test_controller_1
        assert_nil(read_ships_file("Blah"), "Should be nil")
    end

       
end

