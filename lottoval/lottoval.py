class Game:
	def __init__(self, n, c, o):
		self.name = n
		self.cost = c
		self.odds = o
	def expected(self,prize):
		return prize * 1000000 / self.odds / self.cost

games = [Game("Powerball", 292201338.0, 2.0), 
	Game("Mega Millions", 302575350.0, 2.0),
	Game("Texas Lotto", 25827165.0, 1.0)]

print("Enter prizes in millions:")
for g in games:
	p = float(input(g.name + ": "))
	print(g.expected(p))
	print()
