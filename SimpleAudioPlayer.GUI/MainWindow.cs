using System;
using System.Windows.Forms;
using SimpleAudioPlayer.GUI.Properties;
using SimpleAudioPlayer.NET;
using SAP = SimpleAudioPlayer.NET.SimpleAudioPlayer;

namespace SimpleAudioPlayer.GUI
{
    public partial class MainWindow : Form
    {
        private readonly SAP _player;
        private bool _reverse;

        public MainWindow()
        {
            InitializeComponent();
            Icon = Resources.PlayIcon;
            fileMenuStrip.Text = Resources.FileString;
            openMenuStrip.Text = Resources.OpenString;
            exitMenuStrip.Text = Resources.ExitString;

            OpenButton.Text = Resources.OpenButtonString;
            OpenButton.Image = Resources.In16;

            playButton.Text = Resources.PlayString;
            playButton.Image = Resources.Play16;

            stopButton.Text = Resources.StopString;
            stopButton.Image = Resources.Stop16;
            
            _player = new SAP(true);
            _player.StateChanged += OnStateChanged;
            timer.Tick += OnTick;
            DisableControls();
        }

        private void OnStateChanged(object sender, SimpleAudioPlayerEventArgs e)
        {
            if (e.State == SimpleAudioPlayerState.Playing)
            {
                playButton.Text = Resources.PauseString;
                playButton.Image = Resources.Pause16;
            }
            else
            {
                playButton.Text = Resources.PlayString;
                playButton.Image = Resources.Play16;
            }
        }

        private void OnTick(object sender, EventArgs e)
        {
            var position = _player.Position;
            var duration = _player.Duration;

            progressBar.Value = (int) position.TotalSeconds;

            var value = _reverse ? duration.Subtract(position) : position;

            statusLabel.Text = value.ToString(@"mm\:ss");
        }

        private void EnableControls()
        {
            playButton.Enabled = true;
            stopButton.Enabled = true;
            timer.Start();
        }

        private void DisableControls()
        {
            playButton.Enabled = false;
            stopButton.Enabled = false;
            timer.Stop();
        }

        private void OnFileOpen(object sender, EventArgs e)
        {
            if (dialog.ShowDialog() == DialogResult.OK)
            {
                try
                {
                    _player.Source = dialog.FileName;
                    EnableControls();
                    progressBar.Step = 1;
                    progressBar.Maximum = (int)_player.Duration.TotalSeconds;
                    Text = _player.Source;
                }
                catch (Exception)
                {
                    statusLabel.Text = Resources.AudioNaString;
                    DisableControls();
                }
            }
        }

        private void OnExit(object sender, EventArgs e)
        {
            Close();
        }

        protected override void OnFormClosed(FormClosedEventArgs e)
        {
            timer.Dispose();
            _player.Dispose();
            base.OnFormClosed(e);
        }

        private void OnStart(object sender, EventArgs e)
        {
            if (_player.State == SimpleAudioPlayerState.Playing)
                _player.Pause();
            else
                _player.Play();
        }

        private void OnStop(object sender, EventArgs e)
        {
            _player.Stop();
        }

        private void OnReverse(object sender, EventArgs e)
        {
            _reverse = ! _reverse;
        }
    }
}
