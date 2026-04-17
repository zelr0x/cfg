<#
.SYNOPSIS
    Toggles hypervisor on or off.

.DESCRIPTION
    Toggles hypervisor on or off interactively or sets it to the specified
    state non-interactively. Requires admin.

.PARAMETER State
    Positional parameter. State is one of: "On", "Off" or "Auto", case-insensitive.
    "Auto" is the same as "On".

.EXAMPLE
    .\hypervisor-toggle.ps1 on

.EXITCODES
    0  Success
    1  Generic error
    2  State missing in non-interactive mode
    3  Elevation failure (script not run as Administrator)
    10 bcedit failure
#>

[CmdletBinding()]
param(
    # Optional
    [ValidateSet("On", "Auto", "Off", IgnoreCase = $true)]
    [Parameter(Position=0)]
    [string]$state 
)

$isAdmin = ([Security.Principal.WindowsPrincipal] `
    [Security.Principal.WindowsIdentity]::GetCurrent()
).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

$canPrompt = $Host.UI -and $Host.UI.RawUI -and $Host.Name -match "ConsoleHost"


# --- Elevate or exit ---

if (-not $isAdmin) {
    if (-not $canPrompt) {
        Write-Error "Script must be run as Administrator, but no prompt is available for elevation."
        exit 3
    }
    $argList = ($MyInvocation.UnboundArguments | ForEach-Object { '"{0}"' -f $_ }) -join ' '
    $proc = Start-Process powershell `
        -ArgumentList "-ExecutionPolicy Bypass -File `"$PSCommandPath`" $argList" `
        -Verb RunAs `
        -PassThru
    $proc.WaitForExit()
    exit $proc.ExitCode
}


# --- Set mode ---

if ($state) {
    $state = $state.Trim().ToLower()
    if ($state -eq "on") {
        $state = "Auto"
    }
    $bcdResult = bcdedit /set hypervisorlaunchtype $state
    if ($LASTEXITCODE -ne 0) {
        Write-Error "bcdedit failed."
        exit 10
    }
} elseif (-not $canPrompt) {
    Write-Error "State must be provided when no interactive prompt is available."
    exit 2
}


# --- Toggle mode (detect current state then set opposite) ---

$bcd = bcdedit /enum
$oldState = $null

# Find the block for {current}
$block = $false
foreach ($line in $bcd) {
    if ($line -match '^\s*identifier\s+\{current\}') {
        $block = $true
    } elseif ($line -match '^\s*identifier\s+\{') {
        $block = $false
    }

    if ($block -and $line -match 'hypervisorlaunchtype\s+(\w+)') {
        $oldState = $Matches[1]
        break
    }
}

if (-not $oldState) {
    Write-Error "No hypervisorlaunchtype found in {current} section."
    exit 1
}

$oldState = $oldState.Trim().ToLower()
$newState = $null
Write-Host "hypervisorlaunchtype for {current}: $oldState`n"
$newState = if ($oldState -eq "auto") { "Off" } else { "Auto" }
    
$decision = $null
if ($canPrompt) {
    $choices  = "&Yes", "&No (exit)"
    $decision = $Host.UI.PromptForChoice(
        "",
        "Set hypervisorlaunchtype to $newState`?",
        $choices,
        1
    )
} else {
    $decision = 0
}
if ($decision -ne 0) {
    exit 0
}
   
$bcdResult = bcdedit /set hypervisorlaunchtype $newState 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Error "bcdedit failed with $bcdResult."
    exit 10
} else {
    Write-Host "Set hypervisorlaunchtype to $newState."
}


if ($canPrompt) {
    Read-Host "Press Enter to exit"
}

exit 0

