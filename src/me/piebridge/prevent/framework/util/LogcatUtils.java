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

import me.piebridge.prevent.framework.PreventLog;
import me.piebridge.prevent.ui.PreventProvider;

/**
 * Created by thom on 15/8/11.
 */
public class LogcatUtils {

    private static final String CACHE = "/data/system/prevent.log";
    private static final String COMMAND = "/system/bin/logcat -d -v time -f " + CACHE;

    private LogcatUtils() {

    }

    public static void logcat() {
        try {
            PreventLog.d("will execute: " + COMMAND);
            Runtime.getRuntime().exec(COMMAND);
            PreventLog.d("execute complete: " + COMMAND);
        } catch (IOException e) {
            PreventLog.d("exec wrong", e);
        }
    }

    public static void logcat(Context context) {
        try {
            File cache = new File(CACHE);
            if (cache.exists()) {
                PreventLog.d("log size: " + cache.length());
                sendToUi(context, new BufferedInputStream(new FileInputStream(cache)));
                cache.delete();
            } else {
                PreventLog.d("not exist: " + CACHE);
            }
        } catch (IOException e) {
            PreventLog.d("exec wrong", e);
        }
    }

    private static void sendToUi(Context context, InputStream is) throws IOException {
        int length;
        byte[] buffer = new byte[0x300];
        ContentResolver contentResolver = context.getContentResolver();
        String path = new SimpleDateFormat("yyyyMMdd.HH.mm.ss'.log'", Locale.US).format(new Date());
        while ((length = is.read(buffer)) != -1) {
            String line = Base64.encodeToString(buffer, 0, length, Base64.URL_SAFE | Base64.NO_WRAP);
            Uri uri = PreventProvider.CONTENT_URI.buildUpon().appendQueryParameter("path", path)
                    .appendQueryParameter("log", line).build();
            contentResolver.query(uri, null, null, null, null);
        }
        is.close();
    }

}
