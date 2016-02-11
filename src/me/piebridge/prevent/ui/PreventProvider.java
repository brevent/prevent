package me.piebridge.prevent.ui;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.provider.Settings;
import android.util.Base64;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import me.piebridge.prevent.common.FileUtils;

/**
 * Created by thom on 15/7/18.
 */
public class PreventProvider extends ContentProvider {

    public static final Uri CONTENT_URI = Uri.parse("content://me.piebridge.prevent.provider");
    public static final String COLUMN_PACKAGE = Settings.NameValueTable.VALUE;

    @Override
    public boolean onCreate() {
        return true;
    }

    @Override
    public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        String log = uri.getQueryParameter("log");
        if (log != null) {
            saveLog(uri, log);
        }
        return null;
    }

    private void saveLog(Uri uri, String log) {
        String path = uri.getQueryParameter("path");
        String offset = uri.getQueryParameter("offset");
        if (path == null) {
            path = "logcat.log";
        }
        Context context = getContext();
        if (context == null) {
            return;
        }
        if (path.startsWith("boot") && "0".equals(offset)) {
            FileUtils.eraseFiles(context.getExternalCacheDir());
        }
        File dir = context.getExternalCacheDir();
        if (dir == null) {
            UILog.d("cannot find external file");
            return;
        }
        File file = new File(dir, path);
        try {
            FileOutputStream fos = new FileOutputStream(file, true);
            fos.write(Base64.decode(log, Base64.URL_SAFE | Base64.NO_WRAP));
            fos.close();
        } catch (IOException e) {
            UILog.e("cannot save log", e);
        }
    }

    @Override
    public String getType(Uri uri) {
        return null;
    }

    @Override
    public Uri insert(Uri uri, ContentValues values) {
        return null;
    }

    @Override
    public int delete(Uri uri, String selection, String[] selectionArgs) {
        return 0;
    }

    @Override
    public int update(Uri uri, ContentValues values, String selection, String[] selectionArgs) {
        return 0;
    }

}
