#include <stdio.h>
#include <windows.h>
#include <shellapi.h>
#include "SimpleAudioPlayer.h"

#define SAP_STDERR (GetStdHandle(STD_ERROR_HANDLE))
#define SAP_STDOUT (GetStdHandle(STD_OUTPUT_HANDLE))

void sap_printf(HANDLE out, LPCWSTR format, ...)
{
    va_list args;
    int n, mbLen;
    LPWSTR buffer;
    DWORD tmp;
    char *mb;

    va_start(args, format);
    n = _vscwprintf(format, args);
    va_end(args);

    buffer = (LPWSTR)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, (n+1)*sizeof(WCHAR));
    if(!buffer)
        return;

    va_start(args, format);
    _vsnwprintf_s(buffer, n+1, _TRUNCATE, format, args);
    va_end(args);

    if(GetConsoleMode(out, &tmp))
    {
        WriteConsoleW(out, buffer, n, &tmp, NULL);
    }
    else
    {
        mbLen = WideCharToMultiByte(CP_UTF8, 0, buffer, n, NULL, 0, NULL, NULL);
        if(mbLen > 0)
        {
            mb = (char*)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, mbLen);
            if(mb)
            {
                WideCharToMultiByte(CP_UTF8, 0, buffer, n, mb, mbLen, NULL, NULL);
                WriteFile(out, mb, mbLen, &tmp, NULL);
                HeapFree(GetProcessHeap(), 0, mb);
            }
        }
    }
    HeapFree(GetProcessHeap(), 0, buffer);

}

void sap_wait_input()
{
    DWORD tmp;
    INPUT_RECORD ir;
    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);

    if(GetConsoleMode(h, &tmp))
    {
        while(PeekConsoleInputW(h, &ir, 1, &tmp) && ir.EventType != KEY_EVENT)
        {
            ReadConsoleInputW(h, &ir, 1, &tmp);
        }
    }
    else
    {
        WaitForSingleObject(h, INFINITE);
    }
}

int main(void)
{
    int argc;
    LPWSTR *argv;
    ISAPPlayer * player = NULL;
    HRESULT hr;
    LONG duration;

    argv = CommandLineToArgvW(GetCommandLineW(), &argc);

    if(argc != 2)
    {
        sap_printf(SAP_STDERR, L"Usage: %ls <filename>\n", argv[0]);
        return 0;
    }

    hr = OleInitialize(NULL);
    if(FAILED(hr))
        goto cleanup;

    hr = SAPCreatePlayer(&player);
    if(FAILED(hr))
        goto cleanup;

    hr = ISAPPlayer_Open(player, argv[1]);
    if(FAILED(hr))
        goto cleanup;

    hr = ISAPPlayer_Play(player);
    if(FAILED(hr))
        goto cleanup;

    hr = ISAPPlayer_GetDuration(player, &duration);
    if(FAILED(hr))
        goto cleanup;

    sap_printf(SAP_STDOUT, L"Filename: %ls\nDuration: %d:%d\n", argv[1], duration / 60, duration % 60);
    sap_printf(SAP_STDOUT, L"Press any key to exit...\n");
    sap_wait_input();

    ISAPPlayer_Stop(player);

cleanup:
    if(player)
        ISAPPlayer_Release(player);
    OleUninitialize();
    return (int)hr;
}