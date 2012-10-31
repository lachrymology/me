alpha = ("A".."Z").to_a
chars = alpha + 'abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*(){}[]-+=.,;:~'.chars.to_a

class Array
  def rand_el
    self[rand(size)]
  end
end

puts (["  "] + alpha).join(" ")
puts "  " + "--" * alpha.size

alpha.each do |row|
  puts row + "| " + (alpha.collect { |col| chars.rand_el }.join " ")
end
