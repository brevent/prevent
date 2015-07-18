package me.piebridge.forcestopgb.ui;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.provider.Settings;

import me.piebridge.forcestopgb.common.Packages;

/**
 * Created by thom on 15/7/18.
 */
public class Provider extends ContentProvider {

    public static Uri CONTENT_URI = Uri.parse("content://me.piebridge.forcestopgb.provider");
    public static String COLUMN_PACKAGE = Settings.NameValueTable.VALUE;

    @Override
    public boolean onCreate() {
        return true;
    }

    @Override
    public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        String[] columns = {COLUMN_PACKAGE};
        MatrixCursor cursor = new MatrixCursor(columns);
        for (String packageName : Packages.load()) {
            cursor.addRow(new String[] {packageName});
        }
        return cursor;
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
