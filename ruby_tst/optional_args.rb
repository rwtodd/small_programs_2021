# Do optional arguments get evaluated anew on each call,
# or only at function definition time?  Do they get evaluated
# even when the caller provides a value?
#
# Also, can a default value depend on other arguments?
#
# Let's find out!
class Opts
  def self.generate
    puts "Generating something!"
    Random.random_number
  end

  # Here's the method of interest...
  def print_something x=Opts.generate
    puts "It was #{x}"
  end

  # Another question: can the default values
  # depend on the other arguments?
  def print_dependent(a, b=a*2)
    puts "They were #{a} and #{b}!"
  end
end

# Result: It evaluates the optional argument on
# every call when an explicit argument is not given.
# And: default values can depend on previous arguments.
#
# irb(main):029:0> load "optional_args.rb"
# => true
# irb(main):030:0> t = Opts.new
# => #<Opts:0x0000563975a11df0>
# irb(main):031:0> t.print_something :rwtodd
# It was rwtodd
# => nil
# irb(main):032:0> t.print_something
# Generating something!
# It was 0.7945777073369588
# => nil
# irb(main):033:0> t.print_something
# Generating something!
# It was 0.5087246610405544
# => nil
# irb(main):037:0> t.print_dependent(2)
# They were 2 and 4!
# => nil
# irb(main):038:0> t.print_dependent(20)
# They were 20 and 40!
# => nil
