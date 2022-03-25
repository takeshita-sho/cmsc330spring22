class SearsRoebuck
  # Part 1
  def initialize(filename)
    # You may use this block to define your data structures
    # num sold
    @h1={}
    # total price
    @h2={}
   
    # Process each line here!
    File.readlines(filename).each do |line|
        if line =~ /^((?:[A-Z][a-z]*)(?:\s[A-Z][a-z]*)*), \$(\d+)$/
            if @h1[$1]
            @h1[$1] += 1
            else
            @h1[$1] =1
            end
            if @h2[$1]
            @h2[$1] += $2.to_i
            else
            @h2[$1] =$2.to_i
            end
        end
    end
  end 

  # Part 2
  def num_model_sold(model_name)
    if @h1[model_name]
    return @h1[model_name]
    else
    return nil
    end
    if @h1[model_name]
    return @h1[model_name]
    else
    return nil
    end
  end 

  # Part 3
  def avg_selling_price(model_name)
    if @h2[model_name]
    return @h2[model_name]/@h1[model_name]
    else
    return nil
    end
    if @h2[model_name]
    return @h2[model_name]/@h1[model_name]
    else
    return nil
    end
  end

  # Part 4
  def total_sale_amount()
    tot_sale = 0
    @h2.each{|k,v| tot_sale+=v}
    return tot_sale
    tot_sale = 0
    @h2.each{|k,v| tot_sale+=v}
    return tot_sale
  end 
end 

s = SearsRoebuck.new('sales.txt')
puts s.num_model_sold('Fairfield')
puts s.avg_selling_price('Cape Cod')
puts s.total_sale_amount
