package me.piebridge.prevent.ui;

import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.Signature;
import android.net.Uri;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;
import android.text.TextUtils;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.interfaces.RSAPublicKey;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.PreventListUtils;
import me.piebridge.prevent.ui.util.PreventUtils;

/**
 * Created by thom on 15/10/3.
 */
public class SettingsActivity extends PreferenceActivity implements Preference.OnPreferenceChangeListener {

    private boolean licensed = false;

    private RSAPublicKey publicKey;

    public static final String KEY_FORCE_STOP_TIMEOUT = "force_stop_timeout";

    @Override
    @SuppressWarnings("deprecation")
    public void onCreate(Bundle savedInstanceState) {
        final SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
        setTheme(PreventActivity.THEME_LIGHT.equals(sp.getString(PreventActivity.THEME, PreventActivity.THEME_LIGHT)) ? R.style.light : R.style.dark);
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.settings);
        findPreference(KEY_FORCE_STOP_TIMEOUT).setOnPreferenceChangeListener(this);
        // check license
        licensed = false;
        publicKey = getPublicKey();
        byte[] license = new BigInteger(readLicense()).modPow(publicKey.getPublicExponent(), publicKey.getModulus()).toByteArray();
        int i;
        int size = license.length;
        for (i = 0; i < size; ++i) {
            if (license[i] == 0x00) {
                break;
            }
        }
        String name = new String(license, i + 1, license.length - i - 1);
        if (TextUtils.isEmpty(name)) {
            AlertDialog.Builder builder = new AlertDialog.Builder(this);
            builder.setTitle(getString(R.string.app_name) + "(" + BuildConfig.VERSION_NAME + ")");
            builder.setMessage(R.string.no_license);
            builder.setIcon(R.drawable.ic_launcher);
            builder.setOnCancelListener(new DialogInterface.OnCancelListener() {
                @Override
                public void onCancel(DialogInterface dialog) {
                    dialog.dismiss();
                }
            });
            builder.setPositiveButton(getString(android.R.string.ok), new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    dialog.dismiss();
                }
            });
            builder.create().show();
        } else {
            Intent intent = new Intent(PreventIntent.ACTION_CHECK_LICENSE, Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
            intent.putExtra(Intent.EXTRA_USER, name);
            intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
            sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, new BroadcastReceiver() {
                @Override
                public void onReceive(Context context, Intent intent) {
                    if (PreventIntent.ACTION_CHECK_LICENSE.equals(intent.getAction()) && getResultCode() == 1) {
                        licensed = true;
                    }
                }
            }, null, 0, null, null);
        }
    }

    @Override
    public boolean onPreferenceChange(Preference preference, Object newValue) {
        String key = preference.getKey();
        if (KEY_FORCE_STOP_TIMEOUT.equals(key)) {
            if (licensed || !BuildConfig.RELEASE) {
                try {
                    long timeout = Long.valueOf(String.valueOf(newValue));
                    PreventUtils.updateTimeout(this, timeout);
                } catch (NumberFormatException e) {
                    UILog.d(String.valueOf(newValue) + " is not long", e);
                }
                return true;
            } else {
                return false;
            }
        }
        return true;
    }

    private byte[] readLicense() {
        byte[] license = new byte[0x100];
        for (File file : PreventListUtils.getExternalFilesDirs(this)) {
            if (file == null) {
                continue;
            }
            File path = new File(file, "license.key");
            if (path.isFile() && path.canRead()) {
                try {
                    InputStream is = new FileInputStream(path);
                    is.read(license);
                    is.close();
                    return license;
                } catch (IOException e) {
                    UILog.d("cannot get license", e);
                }
            }
        }
        return license;
    }

    private RSAPublicKey getPublicKey() {
        PackageInfo pi;
        try {
            pi = getPackageManager().getPackageInfo(BuildConfig.APPLICATION_ID, PackageManager.GET_SIGNATURES);
        } catch (PackageManager.NameNotFoundException e) {
            UILog.d("cannot get certificate", e);
            return null;
        }
        for (Signature signature : pi.signatures) {
            try {
                final CertificateFactory certFactory = CertificateFactory.getInstance("X.509");
                final ByteArrayInputStream bais = new ByteArrayInputStream(signature.toByteArray());
                final Certificate cert = certFactory.generateCertificate(bais);
                return (RSAPublicKey) cert.getPublicKey();
            } catch (CertificateException e) {
                UILog.d("cannot get certificate", e);
            }
        }
        return null;
    }

}
