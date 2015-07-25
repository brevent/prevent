package me.piebridge.prevent.ui;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.provider.Settings;

import me.piebridge.prevent.ui.util.PackageUtils;
import me.piebridge.prevent.ui.util.PreventListUtils;

/**
 * Created by thom on 15/7/18.
 */
public class PreventProvider extends ContentProvider {

    public static final Uri CONTENT_URI = Uri.parse("content://me.piebridge.forcestopgb.provider");
    public static final String COLUMN_PACKAGE = Settings.NameValueTable.VALUE;

    @Override
    public boolean onCreate() {
        return true;
    }

    @Override
    public Cursor query(Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        String[] columns = {COLUMN_PACKAGE};
        MatrixCursor cursor = new MatrixCursor(columns);
        PackageManager pm = getContext().getPackageManager();
        for (String packageName : PreventListUtils.load()) {
            try {
                ApplicationInfo appInfo = pm.getApplicationInfo(packageName, 0);
                if (PackageUtils.canPrevent(pm, appInfo)) {
                    cursor.addRow(new String[]{packageName});
                }
            } catch (PackageManager.NameNotFoundException e) {
                UILog.d("cannot find package " + packageName, e);
            }
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
