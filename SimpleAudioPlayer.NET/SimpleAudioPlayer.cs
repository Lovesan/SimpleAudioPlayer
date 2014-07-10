using System;
using System.Runtime.InteropServices;

namespace SimpleAudioPlayer.NET
{
    public sealed class SimpleAudioPlayer : IDisposable
    {
        [DllImport("SimpleAudioPlayer.dll", PreserveSig = false)]
        private static extern ISapPlayer SAPCreatePlayer();

        private readonly ISapPlayer _player;
        private readonly SapEventDelegate _delegate;
        private bool _disposed;
        private string _source;

        public SimpleAudioPlayer(bool autoplay)
        {
            _player = SAPCreatePlayer();
            _delegate = OnStateChanged;
            _player.SetCallback(
                Marshal.GetFunctionPointerForDelegate(_delegate),
                IntPtr.Zero);
            _player.SetAutoplay(autoplay);
        }

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

        public void Dispose()
        {
            if (_disposed) return;

            _disposed = true;
            Marshal.FinalReleaseComObject(_player);
        }

        public void Open(string filename)
        {
            _source = filename;
            _player.Open(filename);
        }

        public void Play()
        {
            _player.Play();
        }

        public void Pause()
        {
            _player.Pause();
        }

        public void Stop()
        {
            _player.Stop();
        }

        public void Wait()
        {
            _player.Wait();
        }

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

        public TimeSpan Duration
        {
            get { return TimeSpan.FromSeconds(_player.GetDuration()); }
        }

        public TimeSpan Position
        {
            get { return TimeSpan.FromSeconds(_player.GetPosition()); }
            set { _player.SetPosition((int)value.TotalSeconds); }
        }

        public int Volume
        {
            get { return (int)_player.GetVolume(); }
            set { _player.SetVolume(value); }
        }

        public string Source
        {
            get { return _source; }
            set { Open(value); }
        }

        public bool Autoplay
        {
            get { return _player.GetAutoplay(); }
            set { _player.SetAutoplay(value); }
        }

        public event SimpleAudioPlayerEventHandler StateChanged;
    }
}
