package me.piebridge.prevent.framework.util;

import android.content.ContentResolver;
import android.content.Context;
import android.net.Uri;
import android.util.Base64;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/8/11.
 */
public class LogcatUtils {

    private static final String CACHE = "/data/system/prevent.log";
    private static final String COMMAND = "/system/bin/logcat -d -v time -f " + CACHE;

    private LogcatUtils() {

    }

    public static void logcat(String log) {
        try {
            String command = COMMAND + " " + log;
            PreventLog.d("will execute: " + command);
            Runtime.getRuntime().exec(command);
            PreventLog.d("execute complete: " + command);
            Runtime.getRuntime().exec("/system/bin/sync");
        } catch (IOException e) {
            PreventLog.d("exec wrong", e);
        }
    }

    public static long logcat(Context context, String prefix) {
        File cache = new File(CACHE);
        if (cache.exists()) {
            long size = cache.length();
            PreventLog.d("log size: " + cache.length());
            try {
                sendToUi(context, new BufferedInputStream(new FileInputStream(cache)), prefix);
                PreventLog.d("send to ui successfully");
            } catch (IOException e) {
                PreventLog.d("cannot send log to ui", e);
            }
            cache.delete();
            return size;
        } else {
            PreventLog.d("not exist: " + CACHE);
            return 0L;
        }
    }

    private static void sendToUi(Context context, InputStream is, String prefix) throws IOException {
        int length;
        byte[] buffer = new byte[0x300];
        ContentResolver contentResolver = context.getContentResolver();
        String path = new SimpleDateFormat("yyyyMMdd.HH.mm.ss'.txt'", Locale.US).format(new Date());
        int offset = 0;
        while ((length = is.read(buffer)) != -1) {
            String line = Base64.encodeToString(buffer, 0, length, Base64.URL_SAFE | Base64.NO_WRAP);
            Uri uri = PreventIntent.CONTENT_URI.buildUpon().appendQueryParameter("path", prefix + "." + path)
                    .appendQueryParameter("offset", String.valueOf(offset))
                    .appendQueryParameter("log", line).build();
            contentResolver.query(uri, null, null, null, null);
            offset += length;
        }
        is.close();
    }

}
