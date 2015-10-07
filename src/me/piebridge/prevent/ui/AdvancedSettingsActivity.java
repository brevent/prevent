package me.piebridge.prevent.ui;

import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.text.TextUtils;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.EmailUtils;
import me.piebridge.prevent.ui.util.LicenseUtils;
import me.piebridge.prevent.ui.util.PreventUtils;
import me.piebridge.prevent.ui.util.ThemeUtils;

/**
 * Created by thom on 15/10/3.
 */
public class AdvancedSettingsActivity extends PreferenceActivity implements Preference.OnPreferenceChangeListener {

    public static final String KEY_FORCE_STOP_TIMEOUT = "force_stop_timeout";

    private String license;

    private Preference forceStopTimeout;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        ThemeUtils.setTheme(this);
        super.onCreate(savedInstanceState);
        //noinspection deprecation
        addPreferencesFromResource(R.xml.settings);

        //noinspection deprecation
        forceStopTimeout = findPreference(KEY_FORCE_STOP_TIMEOUT);
        forceStopTimeout.setOnPreferenceChangeListener(this);

        ThemeUtils.fixSmartBar(this);

        // check license
        license = LicenseUtils.getLicense(this);
        Intent intent = new Intent(PreventIntent.ACTION_CHECK_LICENSE, Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
        intent.putExtra(Intent.EXTRA_USER, license);
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
        sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (PreventIntent.ACTION_CHECK_LICENSE.equals(intent.getAction()) && getResultCode() != 1) {
                    alert(getResultData());
                    forceStopTimeout.setEnabled(false);
                }
            }
        }, null, 0, null, null);
    }

    private void alert(String accounts) {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        final String content = "license: " + license + ", accounts: " + accounts;
        builder.setTitle(getString(R.string.app_name) + "(" + BuildConfig.VERSION_NAME + ")");
        if (TextUtils.isEmpty(license)) {
            builder.setMessage(R.string.no_license);
        } else {
            builder.setMessage(getString(R.string.no_valid_license, license, getString(R.string.apply)));
        }
        builder.setIcon(R.drawable.ic_launcher);
        builder.setOnCancelListener(new DialogInterface.OnCancelListener() {
            @Override
            public void onCancel(DialogInterface dialog) {
                dialog.dismiss();
            }
        });
        builder.setNeutralButton(getString(android.R.string.copy), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                dialog.dismiss();
                //noinspection deprecation
                ((android.text.ClipboardManager) getSystemService(Context.CLIPBOARD_SERVICE)).setText(content);
            }
        });
        builder.setPositiveButton(R.string.apply, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                dialog.dismiss();
                EmailUtils.sendEmail(AdvancedSettingsActivity.this, content);
            }
        });
        builder.create().show();
    }

    @Override
    public boolean onPreferenceChange(Preference preference, Object newValue) {
        String key = preference.getKey();
        if (KEY_FORCE_STOP_TIMEOUT.equals(key)) {
            UILog.d("update timeout to " + newValue);
            PreventUtils.updateTimeout(this, String.valueOf(newValue));
        }
        // tricky to fix for android 2.3
        preference.setShouldDisableView(true);
        return true;
    }

}
