using System;
using System.Runtime.InteropServices;

namespace SimpleAudioPlayer.NET
{
    /// <summary>
    /// Simple Audio Player interface for .NET
    /// </summary>
    public sealed class SimpleAudioPlayer : IDisposable
    {
        [DllImport("SimpleAudioPlayer.dll", PreserveSig = false)]
        private static extern ISapPlayer SAPCreatePlayer();

        private readonly ISapPlayer _player;
        private readonly SapEventDelegate _delegate;
        private bool _disposed;
        private string _source;

        /// <summary>
        /// Constructs new <see cref="SimpleAudioPlayer"/> object
        /// </summary>
        /// <param name="autoplay">
        /// <value>true</value> forces player to start playback on file opening
        /// </param>
        public SimpleAudioPlayer(bool autoplay)
        {
            _player = SAPCreatePlayer();
            _delegate = OnStateChanged;
            _player.SetCallback(
                Marshal.GetFunctionPointerForDelegate(_delegate),
                IntPtr.Zero);
            _player.SetAutoplay(autoplay);
        }

        /// <summary>
        /// Constructs new <see cref="SimpleAudioPlayer"/> object
        /// </summary>
        public SimpleAudioPlayer()
            : this(false)
        { }

        ~SimpleAudioPlayer()
        {
            Dispose();
        }

        private void OnStateChanged(IntPtr player, SimpleAudioPlayerState state, IntPtr data)
        {
            var callback = StateChanged;
            if (callback != null)
                callback(this, new SimpleAudioPlayerEventArgs(state));
        }

        /// <summary>
        /// Releases any resources held by <see cref="SimpleAudioPlayer"/> object
        /// </summary>
        public void Dispose()
        {
            if (_disposed) return;

            _disposed = true;
            Marshal.FinalReleaseComObject(_player);
        }

        /// <summary>
        /// Open <paramref name="filename"/> for playback
        /// </summary>
        /// <param name="filename">Audio file to open</param>
        public void Open(string filename)
        {
            _source = filename;
            _player.Open(filename);
        }

        /// <summary>
        /// Starts playback
        /// </summary>
        public void Play()
        {
            _player.Play();
        }

        /// <summary>
        /// Pauses playback
        /// </summary>
        public void Pause()
        {
            _player.Pause();
        }

        /// <summary>
        /// Stops playback
        /// </summary>
        public void Stop()
        {
            _player.Stop();
        }

        /// <summary>
        /// Waits for playback completion
        /// </summary>
        public void Wait()
        {
            _player.Wait();
        }

        /// <summary>
        /// Gets or changes current playback state
        /// </summary>
        public SimpleAudioPlayerState State
        {
            get { return _player.GetState(); }
            set
            {
                switch (value)
                {
                    case SimpleAudioPlayerState.Paused:
                        Pause();
                        break;
                    case SimpleAudioPlayerState.Playing:
                        Play();
                        break;
                    case SimpleAudioPlayerState.Stopped:
                        Stop();
                        break;
                }
            }
        }

        /// <summary>
        /// Gets opened media duration
        /// </summary>
        public TimeSpan Duration
        {
            get { return TimeSpan.FromSeconds(_player.GetDuration()); }
        }

        /// <summary>
        /// Get current position in media stream
        /// </summary>
        public TimeSpan Position
        {
            get { return TimeSpan.FromSeconds(_player.GetPosition()); }
            set { _player.SetPosition((int)value.TotalSeconds); }
        }

        /// <summary>
        /// Gets or sets playback volume, in percents
        /// </summary>
        public int Volume
        {
            get { return (int)_player.GetVolume(); }
            set { _player.SetVolume(value); }
        }

        /// <summary>
        /// Gets or sets media source
        /// </summary>
        public string Source
        {
            get { return _source; }
            set { Open(value); }
        }

        /// <summary>
        /// Gets or sets whether to start playback on <see cref="Open"/>
        /// </summary>
        public bool Autoplay
        {
            get { return _player.GetAutoplay(); }
            set { _player.SetAutoplay(value); }
        }

        /// <summary>
        /// Published whenever playback state changes
        /// </summary>
        public event SimpleAudioPlayerEventHandler StateChanged;
    }
}
