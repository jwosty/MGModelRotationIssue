using System.Windows.Forms;

internal class Program
{
    public static void Main(string[] args)
    {
        // DPI mode - see: https://learn.microsoft.com/en-us/dotnet/desktop/winforms/wfdev-diagnostics/wfac010?view=netdesktop-9.0
        Application.SetHighDpiMode(HighDpiMode.PerMonitorV2);
        using var game = new SheepInc.Core.SheepInc();
        game.Run();
    }
}