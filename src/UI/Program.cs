using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
using CobolAnalyzer.Engine;

namespace CobolAnalyzer.UI
{
    static class Program
    {
        [STAThread]
        public static void Main()
        {
            openConsole();
            int err = CobolAnalyzer.Engine.Main.main();

            if (err == 0 && Interop.Application.mode.IsUI) uiMain();

            if (Interop.Application.consoleOwner.IsSelf && Interop.Application.mode.IsCmdLine)
            {
                Console.WriteLine("\npress any key to quit...\n");
                Console.ReadKey();
            }
        }

        private static void uiMain()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new Form1());
        }

        [DllImport("kernel32.dll",
            EntryPoint = "GetStdHandle",
            SetLastError = true,
            CharSet = CharSet.Auto,
            CallingConvention = CallingConvention.StdCall)]
        private static extern IntPtr GetStdHandle(int nStdHandle);

        [DllImport("kernel32.dll",
            EntryPoint = "AllocConsole",
            SetLastError = true,
            CharSet = CharSet.Auto,
            CallingConvention = CallingConvention.StdCall)]
        private static extern int AllocConsole();

        [DllImport("kernel32.dll",
            EntryPoint = "AttachConsole",
            SetLastError = true,
            CharSet = CharSet.Auto,
            CallingConvention = CallingConvention.StdCall)]
        private static extern int AttachConsole(int pid);

        private const int STD_INPUT_HANDLE = -10;
        private const int STD_OUTPUT_HANDLE = -11;
        private const int STD_ERROR_HANDLE = -12;
        private const int ATTACH_PARENT_PROCESS = -1;

        private static FileStream handle2FileStream(int h, FileAccess access)
        {
            var hptr = GetStdHandle(h);
            var safeh = new SafeFileHandle(hptr, true);
            return new FileStream(safeh, access);
        }

        private static StreamWriter handle2Writer(int h, Encoding enc)
        {
            return new StreamWriter(handle2FileStream(h, FileAccess.Write), enc);
        }

        private static void openConsole()
        {
            if (AttachConsole(ATTACH_PARENT_PROCESS) == 0)
            {
                AllocConsole();
                Interop.Application.consoleOwner = Interop.Application.ConsoleOwner.Self;
            }

            var enc = System.Text.Encoding.GetEncoding(0);
            var outw = handle2Writer(STD_OUTPUT_HANDLE, enc);
            var errw = handle2Writer(STD_ERROR_HANDLE, enc);
            //var inr = new StreamReader(handle2FileStream(STD_INPUT_HANDLE, FileAccess.Read), enc);
            outw.AutoFlush = true;
            errw.AutoFlush = true;
            Console.SetOut(outw);
            Console.SetError(errw);
            //Console.SetIn(inr);
        }
    }
}
