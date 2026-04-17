# Restarts a net adapter. Requires admin.

<#
.SYNOPSIS
    Restarts a network adapter and optionally runs troubleshooting.

.DESCRIPTION
    Disables and re-enables the specified network adapter.
    Optionally flushes DNS and resets Winsock.

.PARAMETER Adapter
    Positional parameter. The name of the network adapter to restart.
    Defaults to "Ethernet" if not specified.

.PARAMETER Flush
    Switch flag. If present, flushes DNS.

.PARAMETER Reset
    Switch flag. If present, resets Winsock.

.PARAMETER Timeout
    Timeout in seconds for disable/enable operations.
    Defaults to 10.

.EXAMPLE
    .\netadapter-restart.ps1 -Adapter "Wi-Fi" -Flush -Reset -Verbose

.EXITCODES
    0  Success
    1  Generic error
    2  Adapter restart failure (disable/enable timeout or error)
    3  Elevation failure (script not run as Administrator)
    4  DNS flush failure
    5  Winsock reset failure
#>

[CmdletBinding()]
param(
    [Parameter(Position=0)]
    [ValidateNotNullOrEmpty()]
    [string]$Adapter = "Ethernet",

    [switch]$Flush,

    [switch]$Reset,

    [ValidateRange(1, 60)]
    [int]$Timeout = 10
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


# --- Restart ---

Write-Verbose "Restarting adapter: $Adapter..."

try {
    Disable-NetAdapter -Name $Adapter -Confirm:$false -ErrorAction Stop
    Write-Progress -Activity "Disabling adapter" -Status "Waiting..." -PercentComplete 0

    $elapsed = 0
    while ($elapsed -lt $Timeout) {
        $status = (Get-NetAdapter -Name $Adapter).Status
        if ($status -eq "Disabled") {
            break
        }
        Start-Sleep -Seconds 1
        $elapsed++
        Write-Progress -Activity "Disabling adapter" -Status "Waiting..." -PercentComplete (($elapsed / $Timeout) * 100)
    }

    Enable-NetAdapter -Name $Adapter -Confirm:$false -ErrorAction Stop
    Write-Progress -Activity "Enabling adapter" -Status "Waiting..." -PercentComplete 0

    $elapsed = 0
    while ($elapsed -lt $Timeout) {
        $status = (Get-NetAdapter -Name $Adapter).Status
        if ($status -eq "Up") {
            break
        }
        Start-Sleep -Seconds 1
        $elapsed++
        Write-Progress -Activity "Enabling adapter" -Status "Waiting..." -PercentComplete (($elapsed / $Timeout) * 100)
    }

    if ($status -ne "Up") {
        Write-Error "Adapter did not enable within $Timeout seconds."
        exit 1
    }

    Write-Verbose "Adapter restarted successfully."
} catch {
    Write-Error "Error: $($_.Exception.Message)"
    exit 2
}

if ($Flush) {
    Write-Verbose "Flushing DNS cache..."
    try {
        ipconfig /flushdns | Out-Null
        Write-Verbose "DNS cache flushed"
    } catch {
        Write-Error "DNS flush failed: $($_.Exception.Message)"
        exit 4
    }
}

if ($Reset) {
    try {
        Write-Verbose "Resetting Winsock..."
        netsh winsock reset | Out-Null
        Write-Verbose "Winsock reset"
    } catch {
        Write-Error "Winsock reset failed: $($_.Exception.Message)"
        exit 5
    }
}

if ($canPrompt) {
    Read-Host "Press Enter to exit"
}

exit 0

