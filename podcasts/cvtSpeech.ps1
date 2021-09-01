[CmdletBinding(SupportsShouldProcess)]
param (
    [Parameter(Mandatory=$true,ValueFromPipeline,Position=1)][string]$Path,
    [Parameter()][string]$BitRate = "20k",
    [Parameter()][string]$StartTime
)

process {
    $inp = Get-Item $Path
    if (-not $inp) {
        write-error "Could not fine file $Path"
        exit 1
    }
    $ofl = $inp.BaseName + ".opus"

    if ($StartTime) {
        $startp = "-ss",$StartTime
    }
    $cmd = "ffmpeg -i $inp -ac 1 -c:a libopus -b:a $BitRate $startp $ofl"
    Write-Verbose $cmd
    if ($PSCmdlet.ShouldProcess($inp, "ffmpeg")) {
        Invoke-Expression $cmd
    }
}
