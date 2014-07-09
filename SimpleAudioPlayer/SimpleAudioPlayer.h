#ifndef __SIMPLE_AUDIO_PLAYER_H__
#define __SIMPLE_AUDIO_PLAYER_H__

#include <oleidl.h>

typedef struct ISAPPlayer ISAPPlayer;

#ifdef __cplusplus
extern "C" {
#endif

typedef enum _SAP_STATE
{
    SAP_STATE_STOPPED,
    SAP_STATE_PAUSED,
    SAP_STATE_PLAYING
} SAP_STATE;

typedef void (CALLBACK *LPSAP_EVENT_CALLBACK)(ISAPPlayer *player, SAP_STATE state, LPVOID data);

HRESULT STDMETHODCALLTYPE SAPCreatePlayer(ISAPPlayer ** ppOut);

#ifdef __cplusplus
}
#endif

#if defined(_MSC_VER) && defined(__cplusplus) && !defined(CINTERFACE)

MIDL_INTERFACE("484b5ca7-1ad8-4635-ae41-9f71a36af0f8")
ISAPPlayer : IUnknown
{
public:
    BEGIN_INTERFACE
        STDMETHOD(Open)(LPCWSTR filename) = 0;
        STDMETHOD(Play)() = 0;
        STDMETHOD(Pause)() = 0;
        STDMETHOD(Stop)() = 0;
        STDMETHOD(Wait)() = 0;
        STDMETHOD(SetCallback)(LPSAP_EVENT_CALLBACK callback, LPVOID data) = 0;
        STDMETHOD(GetState)(SAP_STATE *pState) = 0;
        STDMETHOD(GetVolume)(LONG *pVolume) = 0;
        STDMETHOD(SetVolume)(LONG volume) = 0;
        STDMETHOD(GetDuration)(LONG *pDuration) = 0;
        STDMETHOD(GetPosition)(LONG *pPosition) = 0;
        STDMETHOD(SetPosition)(LONG position) = 0;
    END_INTERFACE
};

#else

typedef struct ISAPPlayerVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ISAPPlayer * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ISAPPlayer * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ISAPPlayer * This);

        HRESULT ( STDMETHODCALLTYPE *Open)(ISAPPlayer * This, LPCWSTR filename);

        HRESULT ( STDMETHODCALLTYPE *Play)(ISAPPlayer * This);

        HRESULT ( STDMETHODCALLTYPE *Pause)(ISAPPlayer * This);

        HRESULT ( STDMETHODCALLTYPE *Stop)(ISAPPlayer * This);

        HRESULT ( STDMETHODCALLTYPE *Wait)(ISAPPlayer * This);

        HRESULT ( STDMETHODCALLTYPE *SetCallback)(
            ISAPPlayer * This,
            LPSAP_EVENT_CALLBACK callback,
            LPVOID data);

        HRESULT ( STDMETHODCALLTYPE *GetState)(ISAPPlayer * This, SAP_STATE *pState);

        HRESULT ( STDMETHODCALLTYPE *GetVolume)(ISAPPlayer * This, LONG *pVolume);
        
        HRESULT ( STDMETHODCALLTYPE *SetVolume)(ISAPPlayer * This, LONG volume);

        HRESULT ( STDMETHODCALLTYPE *GetDuration)(ISAPPlayer * This, LONG *pDuration);

        HRESULT ( STDMETHODCALLTYPE *GetPosition)(ISAPPlayer * This, LONG *pPosition);

        HRESULT ( STDMETHODCALLTYPE *SetPosition)(ISAPPlayer * This, LONG position);
        
        END_INTERFACE
    } ISAPPlayerVtbl;

    struct ISAPPlayer
    {
        CONST_VTBL struct ISAPPlayerVtbl *lpVtbl;
    };

#define ISAPPlayer_QueryInterface(This, riid, ppvObject) \
    This->lpVtbl->QueryInterface(This, riid, ppvObject)

#define ISAPPlayer_AddRef(This) \
    This->lpVtbl->AddRef(This)
    
#define ISAPPlayer_Release(This) \
    This->lpVtbl->Release(This)

#define ISAPPlayer_Open(This, filename) \
    This->lpVtbl->Open(This, filename)

#define ISAPPlayer_Play(This) \
    This->lpVtbl->Play(This)

#define ISAPPlayer_Pause(This) \
    This->lpVtbl->Pause(This)

#define ISAPPlayer_Stop(This) \
    This->lpVtbl->Stop(This)

#define ISAPPlayer_Wait(This) \
    This->lpVtbl->Wait(This)

#define ISAPPlayer_SetCallback(This, callback, data) \
    This->lpVtbl->SetCallbak(This, callback, data)

#define ISAPPlayer_GetState(This, pState) \
    This->lpVtbl->GetState(This, pState)

#define ISAPPlayer_GetVolume(This, pVolume) \
    This->lpVtbl->GetVolume(This, pVolume)

#define ISAPPlayer_SetVolume(This, volume) \
    This->lpVtbl->SetVolume(This, volume)

#define ISAPPlayer_GetDuration(This, pDuration) \
    This->lpVtbl->GetDuration(This, pDuration)

#define ISAPPlayer_GetPosition(This, pPosition) \
    This->lpVtbl->GetPosition(This, pPosition)

#define ISAPPlayer_SetPosition(This, position) \
    This->lpVtbl->SetPosition(This, position)

#endif // __cplusplus

#endif // __SIMPLE_AUDIO_PLAYER_H__
