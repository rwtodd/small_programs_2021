class Game
	def initialize(name, odds, cost)
		@name = name
		@odds = odds
		@cost = cost
	end

	def expected(prize)
		prize * 1000000 / @odds / @cost
	end

	attr_reader :name
end

games = [Game.new("Powerball", 292201338.0, 2.0), 
	Game.new("Mega Millions", 302575350.0, 2.0),
	Game.new("Texas Lotto", 25827165.0, 1.0)]

puts "Enter game prizes in millions:"
games.each do |g|
    print "#{g.name}: "
    prize = gets.chop.to_f
    puts (g.expected prize) ; puts
end
