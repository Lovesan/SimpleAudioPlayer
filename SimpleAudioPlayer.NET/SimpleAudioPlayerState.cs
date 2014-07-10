namespace SimpleAudioPlayer.NET
{
    /// <summary>
    /// Playback state of <see cref="SimpleAudioPlayer"/>
    /// </summary>
    public enum SimpleAudioPlayerState
    {
        /// <summary>
        /// Playback is stopped
        /// </summary>
        Stopped,
        /// <summary>
        /// Playback is paused
        /// </summary>
        Paused,
        /// <summary>
        /// Playback is in progress
        /// </summary>
        Playing
    }
}
