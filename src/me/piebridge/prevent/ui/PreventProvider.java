package me.piebridge.prevent.ui;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.database.MatrixCursor;
import android.net.Uri;
import android.provider.Settings;
import android.support.v4.app.NotificationCompat;

import java.util.Set;

import me.piebridge.prevent.ui.util.PackageUtils;
import me.piebridge.prevent.ui.util.PreventListUtils;

import me.piebridge.forcestopgb.R;

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
        Set<String> packages = PreventListUtils.load();
        for (String packageName : packages) {
            try {
                ApplicationInfo appInfo = pm.getApplicationInfo(packageName, 0);
                if (PackageUtils.canPrevent(pm, appInfo)) {
                    cursor.addRow(new String[]{packageName});
                }
            } catch (PackageManager.NameNotFoundException e) {
                UILog.d("cannot find package " + packageName, e);
            }
        }
        if (packages.isEmpty()) {
            notifyNoPrevents(getContext());
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

    private void notifyNoPrevents(Context context) {
        Intent open = new Intent(context, PreventActivity.class);
        open.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        PendingIntent activity = PendingIntent.getActivity(context, 0, open, PendingIntent.FLAG_UPDATE_CURRENT);

        Notification notification = new NotificationCompat.Builder(context)
                .setAutoCancel(true)
                .setContentTitle(context.getText(R.string.app_name))
                .setContentText(context.getText(R.string.no_prevents))
                .setTicker(context.getText(R.string.app_name))
                .setSmallIcon(R.drawable.ic_launcher)
                .setContentIntent(activity).build();

        NotificationManager nm = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        nm.notify(0, notification);
    }

}
