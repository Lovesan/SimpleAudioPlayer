#include <new>
#include <windows.h>
#include <dshow.h>
#include "ComUtils.hpp"
#include "SimpleAudioPlayer.h"

#define SAP_WM_NOTIFY (WM_APP+1)
#define SAP_100NS_PER_MS (10 * 1000 * 1000)

class COM_NO_VTABLE SAPPlayer
    : public ISAPPlayer
{
private:
    static const LPCWSTR ClassName;
    ComPtr<IFilterGraph2> _graph;
    ComPtr<IBaseFilter> _source;
    ComPtr<IBaseFilter> _sink;
    ComPtr<IMediaControl> _ctrl;
    ComPtr<IMediaEventEx> _evt;
    ComPtr<IBasicAudio> _audio;
    ComPtr<IMediaSeeking> _seek;
    LPSAP_EVENT_CALLBACK _callback;
    LPVOID _callbackData;
    HWND _hwnd;
    BOOL _stopped;

    static LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
    {
        if(msg == SAP_WM_NOTIFY)
        {
            SAPPlayer *p = (SAPPlayer*)lParam;
            if(p)
                p->ProcessEvents();
            return 0;
        }
        return DefWindowProc(hwnd, msg, wParam, lParam);
    }

    void ProcessEvents()
    {
        LONG_PTR p1, p2;
        LONG evt;
        LONGLONG p = 0;

        if(_evt)
        {
            while(SUCCEEDED(_evt->GetEvent(&evt, &p1, &p2, 0)))
            {
                switch(evt)
                {
                case EC_COMPLETE:
                case EC_USERABORT:
                case EC_ERRORABORT:
                    _ctrl->Stop();
                    _seek->SetPositions(&p, AM_SEEKING_AbsolutePositioning, NULL, AM_SEEKING_NoPositioning);
                    _stopped = TRUE;
                    if(evt == EC_COMPLETE && _callback)
                        _callback(this, SAP_STATE_STOPPED, _callbackData);
                    break;
                }
                _evt->FreeEventParams(evt, p1, p2);
            }
        }
    }

public:
    BEGIN_INTERFACE_MAP
        INTERFACE_MAP_ENTRY(ISAPPlayer)
    END_INTERFACE_MAP

    SAPPlayer()
    {
        _callback = NULL;
        _hwnd = NULL;
        _stopped = TRUE;
    }

    virtual ~SAPPlayer()
    {
        if(IsWindow(_hwnd))
            DestroyWindow(_hwnd);
    }

    STDMETHODIMP Initialize()
    {
        HRESULT hr = S_OK;
        WNDCLASSEXW wc = {0};

        wc.cbSize = sizeof(wc);
        wc.hInstance = GetModuleHandle(NULL);
        wc.lpfnWndProc = WndProc;
        wc.lpszClassName = ClassName;
        
        RegisterClassEx(&wc);

        _hwnd = CreateWindowExW(
                    0,
                    ClassName,
                    L"",
                    0,
                    0,
                    0,
                    0,
                    0,
                    HWND_MESSAGE,
                    NULL,
                    wc.hInstance,
                    NULL);

        if(!IsWindow(_hwnd))
            return HRESULT_FROM_WIN32(GetLastError());

        return hr;
    }

    STDMETHODIMP Open(LPCWSTR filename)
    {
        HRESULT hr;

        if(!filename)
            return E_POINTER;

        if(!IsWindow(_hwnd))
            return VFW_E_WRONG_STATE;

        _graph.Release();
        _source.Release();
        _ctrl.Release();
        _evt.Release();
        _audio.Release();
        _seek.Release();
        _sink.Release();

        _stopped = TRUE;

        V_HR_ASSERT(_graph.CoCreateInstance(CLSID_FilterGraph), "Unable to create filter graph");
        
        V_HR_MSG(_graph->AddSourceFilter(filename, L"Source", &_source), "Unable to open file");

        V_HR_ASSERT(_sink.CoCreateInstance(CLSID_DSoundRender), "Unable to create DirectSound renderer");

        V_HR_ASSERT(_graph->AddFilter(_sink, L"DSound"), "Unable to add DSound filter");

        ComPtr<IEnumPins> enumPins;
        ComPtr<IPin> pin;
        ULONG cPins;
        V_HR(_source->EnumPins(&enumPins));
        BOOL atLeastOne = FALSE;
        while(SUCCEEDED(enumPins->Next(1, &pin, &cPins)) && cPins)
        {
            hr = _graph->RenderEx(
                          pin,
                          AM_RENDEREX_RENDERTOEXISTINGRENDERERS,
                          NULL);
            atLeastOne = atLeastOne || SUCCEEDED(hr);
            pin.Release();
        }
        enumPins.Release();
        if(!atLeastOne)
          return HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);

        V_HR_MSG(_graph.QueryInterface(&_audio), "Unable to get audio filter");
        
        V_HR_MSG(_graph.QueryInterface(&_ctrl), "Unable to get media control");

        V_HR_MSG(_graph.QueryInterface(&_evt), "Unable to get media event");

        V_HR_MSG(_graph.QueryInterface(&_seek), "Unable to get media seeking");

        V_HR_MSG(_seek->SetTimeFormat(&TIME_FORMAT_MEDIA_TIME), "Unable to set time format");

        V_HR_ASSERT(
            _evt->SetNotifyWindow((OAHWND)_hwnd, SAP_WM_NOTIFY, (LONG_PTR)this),
            "Unable to set notify window");

        V_HR_ASSERT(_evt->SetNotifyFlags(0), "Unable to set notifcation flags");
        
        return hr;
    }

    STDMETHODIMP Play()
    {
        HRESULT hr;
        OAFilterState state;

        if(!_ctrl)
            return VFW_E_WRONG_STATE;

        V_HR(_ctrl->GetState(INFINITE, &state));

        if(state != State_Running)
        {
            V_HR(_ctrl->Run());
            _stopped = FALSE;
            if(_callback)
                _callback(this, SAP_STATE_PLAYING, _callbackData);
        }
        
        return hr;
    }

    STDMETHODIMP Pause()
    {
        HRESULT hr;
        OAFilterState state;

        if(!_ctrl)
            return VFW_E_WRONG_STATE;

        V_HR(_ctrl->GetState(INFINITE, &state));
        if(state == State_Running)
        {
            V_HR(_ctrl->Pause());
            if(_callback)
                _callback(this, SAP_STATE_PAUSED, _callbackData);
        }

        return hr;
    }

    STDMETHODIMP Stop()
    {
        HRESULT hr;
        OAFilterState state;
        LONGLONG p = 0;

        if(!_ctrl)
            return VFW_E_WRONG_STATE;

        V_HR(_ctrl->GetState(INFINITE, &state));

        if(!_stopped && state != SAP_STATE_STOPPED)
        {
            V_HR(_ctrl->Stop());
            V_HR(_seek->SetPositions(&p, AM_SEEKING_AbsolutePositioning, NULL, AM_SEEKING_NoPositioning));
            _stopped = TRUE;
            if(_callback)
                _callback(this, SAP_STATE_STOPPED, _callbackData);
        }

        return hr;
    }

    STDMETHODIMP Wait()
    {
        HRESULT hr = S_OK;
        MSG msg;

        if(!_evt)
            return VFW_E_WRONG_STATE;

        while(!_stopped && GetMessage(&msg, NULL, 0, 0) > 0)
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        
        return hr;
    }

    STDMETHODIMP SetCallback(LPSAP_EVENT_CALLBACK callback, LPVOID data)
    {
        _callback = callback;
        _callbackData = data;
        return S_OK;
    }

    STDMETHODIMP GetState(SAP_STATE *pState)
    {
        HRESULT hr;
        OAFilterState state;

        if(!pState)
            return E_POINTER;

        if(!_ctrl)
            return VFW_E_WRONG_STATE;

        V_HR(_ctrl->GetState(INFINITE, &state));

        switch(state)
        {
        case State_Running:
            *pState = SAP_STATE_PLAYING;
            break;
        case State_Paused:
            *pState = SAP_STATE_PAUSED;
            break;
        default:
            *pState = SAP_STATE_STOPPED;
        }

        return S_OK;
    }

    STDMETHODIMP GetVolume(LONG *pVolume)
    {
        HRESULT hr;
        LONG volume;

        if(!pVolume)
            return E_POINTER;

        if(!_audio)
            return VFW_E_WRONG_STATE;

        V_HR(_audio->get_Volume(&volume));

        *pVolume = (volume + 10000) / 100;

        return hr;
    }

    STDMETHODIMP SetVolume(LONG volume)
    {
        HRESULT hr;

        volume = volume < 0 ? 0 : volume;
        volume = volume > 100 ? 100 : volume;

        if(!_audio)
            return VFW_E_WRONG_STATE;
        
        V_HR(_audio->put_Volume(volume * 100 - 10000));

        return hr;
    }

    STDMETHODIMP GetDuration(LONG *pDuration)
    {
        HRESULT hr;
        LONGLONG duration;

        if(!pDuration)
            return E_POINTER;

        if(!_seek)
            return VFW_E_WRONG_STATE;

        V_HR(_seek->GetDuration(&duration));

        *pDuration = (LONG)(duration / SAP_100NS_PER_MS);

        return hr;
    }

    STDMETHODIMP GetPosition(LONG *pPosition)
    {
        HRESULT hr;
        LONGLONG position;

        if(!pPosition)
            return E_POINTER;

        if(!_seek)
            return VFW_E_WRONG_STATE;

        V_HR(_seek->GetCurrentPosition(&position));

        *pPosition = (LONG)(position / SAP_100NS_PER_MS);

        return hr;
    }

    STDMETHODIMP SetPosition(LONG position)
    {
        HRESULT hr;
        LONGLONG p = position * SAP_100NS_PER_MS;

        if(!_seek)
            return VFW_E_WRONG_STATE;

        V_HR(_seek->SetPositions(&p, AM_SEEKING_AbsolutePositioning, NULL, AM_SEEKING_NoPositioning));

        return hr;
    }
};

const LPCWSTR SAPPlayer::ClassName = L"SAPPlayer";

HRESULT STDMETHODCALLTYPE SAPCreatePlayer(ISAPPlayer ** ppOut)
{
    HRESULT hr;
    SAPPlayer * player;

    if(!ppOut)
        return E_POINTER;
    *ppOut = NULL;

    player = new (std::nothrow) ComObject<SAPPlayer>();
    if(!player)
        return E_OUTOFMEMORY;

    hr = player->Initialize();
    if(FAILED(hr))
    {
        delete player;
        return hr;
    }
    player->AddRef();    
    *ppOut = player;
    return S_OK;
}
