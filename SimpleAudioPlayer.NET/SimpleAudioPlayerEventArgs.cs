using System;

namespace SimpleAudioPlayer.NET
{
    public class SimpleAudioPlayerEventArgs : EventArgs
    {
        public SimpleAudioPlayerState State { get; private set; }

        public SimpleAudioPlayerEventArgs(SimpleAudioPlayerState state)
        {
            State = state;
        }
    }
}
