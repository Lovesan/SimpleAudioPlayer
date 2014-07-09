using System;
using System.Runtime.InteropServices;

namespace SimpleAudioPlayer.NET
{
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    [Guid("484b5ca7-1ad8-4635-ae41-9f71a36af0f8")]
    [ComImport]
    internal interface ISapPlayer
    {
        void Open([In, MarshalAs(UnmanagedType.LPWStr)] string filename);

        void Play();

        void Pause();

        void Stop();

        void Wait();

        void SetCallback(IntPtr function, IntPtr data);

        SimpleAudioPlayerState GetState();

        long GetVolume();

        void SetVolume(long volume);

        long GetDuration();

        long GetPosition();

        void SetPosition(long position);
    }
}
