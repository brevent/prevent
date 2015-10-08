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
import android.util.Base64;

import java.util.Arrays;
import java.util.Collection;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.EmailUtils;
import me.piebridge.prevent.ui.util.LicenseUtils;
import me.piebridge.prevent.ui.util.PreventUtils;
import me.piebridge.prevent.ui.util.RecreateUtils;
import me.piebridge.prevent.ui.util.ThemeUtils;

/**
 * Created by thom on 15/10/3.
 */
public class AdvancedSettingsActivity extends PreferenceActivity implements Preference.OnPreferenceChangeListener {

    private String license;

    private String accounts;

    private Preference forceStopTimeout;

    private Preference destroyProcesses;

    private static Collection<String> KEYS_NEED_LICENSE = Arrays.asList(
            PreventIntent.KEY_DESTROY_PROCESSES
    );

    @Override
    public void onCreate(Bundle savedInstanceState) {
        ThemeUtils.setTheme(this);
        super.onCreate(savedInstanceState);
        //noinspection deprecation
        addPreferencesFromResource(R.xml.settings);

        //noinspection deprecation
        forceStopTimeout = findPreference(PreventIntent.KEY_FORCE_STOP_TIMEOUT);
        forceStopTimeout.setOnPreferenceChangeListener(this);

        //noinspection deprecation
        destroyProcesses = findPreference(PreventIntent.KEY_DESTROY_PROCESSES);
        destroyProcesses.setOnPreferenceChangeListener(this);

        // check license
        license = LicenseUtils.getLicense(this);
        Intent intent = new Intent(PreventIntent.ACTION_CHECK_LICENSE, Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
        intent.putExtra(Intent.EXTRA_USER, license);
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
        sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (PreventIntent.ACTION_CHECK_LICENSE.equals(intent.getAction()) && getResultCode() != 1) {
                    accounts = getResultData();
                }
            }
        }, null, 0, null, null);
    }

    private void alert() {
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
                //noinspection deprecation
                ((android.text.ClipboardManager) getSystemService(Context.CLIPBOARD_SERVICE)).setText(content);
            }
        });
        builder.setPositiveButton(R.string.apply, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                EmailUtils.sendEmail(AdvancedSettingsActivity.this, content);
            }
        });
        builder.setNegativeButton(android.R.string.paste,new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                String plain =  ((android.text.ClipboardManager) getSystemService(Context.CLIPBOARD_SERVICE)).getText().toString();
                byte[] key = Base64.decode(plain, Base64.DEFAULT);
                if (!TextUtils.isEmpty(LicenseUtils.getLicense(key))) {
                    LicenseUtils.saveLicense(AdvancedSettingsActivity.this, key);
                    RecreateUtils.recreate(AdvancedSettingsActivity.this);
                }
            }
        });
        builder.create().show();
    }

    @Override
    public boolean onPreferenceChange(Preference preference, Object newValue) {
        String key = preference.getKey();
        if (!TextUtils.isEmpty(accounts) && KEYS_NEED_LICENSE.contains(key)) {
            alert();
            return false;
        }
        if (PreventIntent.KEY_FORCE_STOP_TIMEOUT.equals(key)) {
            UILog.d("update timeout to " + newValue);
            Bundle bundle = new Bundle();
            bundle.putLong(PreventIntent.KEY_FORCE_STOP_TIMEOUT, Long.valueOf(String.valueOf(newValue)));
            PreventUtils.updateConfiguration(this, bundle);
        } else if (PreventIntent.KEY_DESTROY_PROCESSES.equals(key)) {
            UILog.d("update destroy processes to " + newValue);
            Bundle bundle = new Bundle();
            bundle.putBoolean(PreventIntent.KEY_DESTROY_PROCESSES, (Boolean) newValue);
            PreventUtils.updateConfiguration(this, bundle);
        }
        // tricky to fix for android 2.3
        preference.setShouldDisableView(true);
        return true;
    }

}
