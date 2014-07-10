using System;

namespace SimpleAudioPlayer.NET
{
    /// <summary>
    /// Event parameters for <see cref="SimpleAudioPlayer"/> events.
    /// </summary>
    public class SimpleAudioPlayerEventArgs : EventArgs
    {
        /// <summary>
        /// New player state
        /// </summary>
        public SimpleAudioPlayerState State { get; private set; }

        /// <summary>
        /// Constructs new instance of <see cref="SimpleAudioPlayerEventArgs"/>
        /// </summary>
        /// <param name="state">New player state</param>
        public SimpleAudioPlayerEventArgs(SimpleAudioPlayerState state)
        {
            State = state;
        }
    }
}
