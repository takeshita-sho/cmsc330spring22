class WaitingTime
    def initialize(filename)
      @times = Hash.new(0)
      IO.foreach(filename) {|line| if line =~ /([A-Z][a-z]+), ([A-Z][a-z]+), ([0-9]+):([0-9]+)/ # () - for grouping to call with $#
                                @times["#{2} #{$1}"] += 60*($3.to_i) + ($4.to_i)
      end
      } 

    end
    
    def student_waited_for(student_name)
      @times[student_name]
    end
    
    def total_wait_time()
      total = 0
      @times.keys.each {|k| total += student_waited_for(k)} 
    end
end

class DuckSorter
    def initialize(filename)
      IO.foreach(filename) { |line| 
        raise "unimplemented" 
      }
    end

    def get_attribute(name) 
      raise "unimplemented" 
    end

    def search(attribute)
      raise "unimplemented" 
    end
end