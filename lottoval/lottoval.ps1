class Game {
   [string]$name
   [double]$odds
   [double]$cost

   Game([string]$n, [double]$o, [double]$c) { $this.name = $n; $this.odds = $o; $this.cost = $c }

   [double] Expected([double]$prize) { return  ($prize*1000000 / $this.odds / $this.cost) }
}

$games = @([Game]::new("Powerball",292201338.0, 2.0),
   [Game]::new("Mega Millions",302575350.0, 2.0),
   [Game]::new("Texas Lotto",25827165.0, 1.0))

echo "Enter prizes in millions..."
foreach($g in $games) {
   [double]$prize = Read-Host -Prompt $g.name
   echo ("Expected value: {0:F2}" -f $g.Expected($prize))
   echo ""
}


  