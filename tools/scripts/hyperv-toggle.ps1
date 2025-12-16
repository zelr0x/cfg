# Toggles hypervisor on or off. Requires admin.

param(
    # Optional
    [ValidateSet("On", "Auto", "Off", IgnoreCase = $true)]
    [string]$state 
)

$isAdmin = ([Security.Principal.WindowsPrincipal] `
    [Security.Principal.WindowsIdentity]::GetCurrent()
).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

$canPrompt = $Host.UI -and $Host.UI.RawUI -and $Host.Name -match "ConsoleHost"


# --- Elevate or exit ---

if (-not $isAdmin) {
    if (-not $canPrompt) {
        Write-Host "ERROR: Script must be run as Administrator, but no prompt is available for elevation."
        return 3
    }
    $argList = ($MyInvocation.UnboundArguments | ForEach-Object { '"{0}"' -f $_ }) -join ' '
    $proc = Start-Process powershell `
        -ArgumentList "-ExecutionPolicy Bypass -File `"$PSCommandPath`" $argList" `
        -Verb RunAs `
        -PassThru
    $proc.WaitForExit()
    return $proc.ExitCode
}


# --- Set mode ---

if ($state) {
    $state = $state.Trim().ToLower()
    if ($state -eq "on") {
        $state = "Auto"
    }
    $bcdResult = bcdedit /set hypervisorlaunchtype $state
    if ($LASTEXITCODE -ne 0) {
        Write-Host "ERROR: bcdedit failed."
        return 10
    }
} elseif (-not $canPrompt) {
    Write-Host "ERROR: state must be provided when no interactive prompt is available."
    return 2
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
    Write-Host "No hypervisorlaunchtype found in {current} section."
    return 1
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
    return 0
}
   
$bcdResult = bcdedit /set hypervisorlaunchtype $newState 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: bcdedit failed with $bcdResult."
    return 10
} else {
    Write-Host "Set hypervisorlaunchtype to $newState."
}


if ($canPrompt) {
    Read-Host "Press Enter to exit"
}
return 0
