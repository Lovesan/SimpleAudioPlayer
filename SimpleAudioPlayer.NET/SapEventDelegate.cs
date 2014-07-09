using System;

namespace SimpleAudioPlayer.NET
{
    internal delegate void SapEventDelegate(IntPtr player, SimpleAudioPlayerState state, IntPtr data);
}
