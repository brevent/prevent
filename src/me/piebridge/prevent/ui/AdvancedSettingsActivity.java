package me.piebridge.prevent.ui;

import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.text.TextUtils;
import android.widget.Toast;

import java.util.Collection;
import java.util.Collections;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.DeprecatedUtils;
import me.piebridge.prevent.ui.util.LicenseUtils;
import me.piebridge.prevent.ui.util.PreventUtils;
import me.piebridge.prevent.ui.util.RecreateUtils;
import me.piebridge.prevent.ui.util.ThemeUtils;

/**
 * Created by thom on 15/10/3.
 */
public class AdvancedSettingsActivity extends PreferenceActivity implements Preference.OnPreferenceChangeListener, Preference.OnPreferenceClickListener {

    private String license;

    private String accounts;

    private AlertDialog dialog;

    private static Collection<String> KEYS_NEED_LICENSE = Collections.singletonList(
            PreventIntent.KEY_DESTROY_PROCESSES
    );

    @Override
    public void onCreate(Bundle savedInstanceState) {
        ThemeUtils.setTheme(this);
        super.onCreate(savedInstanceState);
        DeprecatedUtils.addPreferencesFromResource(this, R.xml.settings);

        Preference forceStopTimeout = DeprecatedUtils.findPreference(this, PreventIntent.KEY_FORCE_STOP_TIMEOUT);
        forceStopTimeout.setOnPreferenceChangeListener(this);
        forceStopTimeout.setOnPreferenceClickListener(this);

        Preference destroyProcesses = DeprecatedUtils.findPreference(this, PreventIntent.KEY_DESTROY_PROCESSES);
        destroyProcesses.setOnPreferenceChangeListener(this);

        // check license
        if (BuildConfig.DONATE) {
            checkAccounts();
        }
    }

    private void checkAccounts() {
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

    @Override
    protected void onResume() {
        super.onResume();
        if (BuildConfig.DONATE) {
            checkLicense();
        }
    }

    private boolean checkLicense() {
        if (LicenseUtils.importLicenseFromClipboard(this)) {
            Toast.makeText(this, R.string.licensed, Toast.LENGTH_LONG).show();
            if (dialog != null) {
                dialog.dismiss();
                dialog = null;
            }
            RecreateUtils.recreate(this);
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean onPreferenceChange(Preference preference, Object newValue) {
        String key = preference.getKey();
        if (BuildConfig.DONATE && !LicenseUtils.isInAppLicensed() && !TextUtils.isEmpty(accounts) && KEYS_NEED_LICENSE.contains(key)) {
            LicenseUtils.requestLicense(this, license, accounts);
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

    @Override
    public boolean onPreferenceClick(Preference preference) {
        String key = preference.getKey();
        if (BuildConfig.DONATE && !LicenseUtils.isInAppLicensed() && !TextUtils.isEmpty(accounts) && KEYS_NEED_LICENSE.contains(key)) {
            LicenseUtils.requestLicense(this, license, accounts);
            return true;
        } else {
            return false;
        }
    }

}
