package me.piebridge.forcestopgb;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import android.app.ActivityManager.RunningAppProcessInfo;
import android.app.ProgressDialog;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.AsyncTask;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;

public class Adapter extends ArrayAdapter<AppInfo> {
	private LayoutInflater inflater;
	private MainActivity mContext;
	private PackageManager pm;
	private static Map<String, String> labels = new HashMap<String, String>();

	public Adapter(MainActivity context) {
		super(context, R.layout.item);
		mContext = context;
		pm = mContext.getPackageManager();
		inflater = LayoutInflater.from(context);
	}

	public Adapter(MainActivity context, Set<String> names) {
		this(context);
		addAll(names, false);
	}

	public Adapter(MainActivity context, Set<String> names, boolean nocache) {
		this(context);
		addAll(names, nocache);
	}

	public void addAll(final Set<String> names, final boolean nocache) {
		new AsyncTask<Void, Integer, TreeSet<AppInfo>>() {
			ProgressDialog dialog;

			@Override
			protected void onPreExecute() {
				if (nocache) {
					labels.clear();
					dialog = new ProgressDialog(mContext);
					dialog.setMessage(mContext.getString(R.string.loading));
					dialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
					dialog.setCancelable(false);
					dialog.setMax(names.size());
					dialog.show();
				}
			}

			@Override
			protected TreeSet<AppInfo> doInBackground(Void... params) {
				Map<String, Set<Integer>> running = mContext.getRunningProcesses();
				TreeSet<AppInfo> applications = new TreeSet<AppInfo>();
				int i = 1;
				for (String name : names) {
					try {
						publishProgress(++i);
						ApplicationInfo info = pm.getApplicationInfo(name, 0);
						if (!info.enabled) {
							continue;
						}
						if (nocache) {
							String label = info.loadLabel(pm).toString();
							labels.put(name, label);
							applications.add(new AppInfo(name, label, running.get(name)).flags(info.flags));
						} else {
							String label = labels.get(name);
							if (label == null) {
								label = info.loadLabel(pm).toString();
							}
							applications.add(new AppInfo(name, label, running.get(name)));
						}
					} catch (NameNotFoundException e) {
						e.printStackTrace();
					}
				}
				return applications;
			}

			@Override
			protected void onProgressUpdate(Integer... progress) {
				if (nocache) {
					dialog.setProgress(progress[0]);
				}
			}

			@Override
			protected void onPostExecute(TreeSet<AppInfo> applications) {
				for (AppInfo application : applications) {
					add(application);
				}
				try {
					if (nocache) {
						dialog.dismiss();
					}
				} catch (Exception e) {
				}
			}
		}.execute();
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View view = convertView;
		if (view == null) {
			view = inflater.inflate(R.layout.item, null, true);
			ViewHolder viewHolder = new ViewHolder();
			viewHolder.checkView = (CheckBox) view.findViewById(R.id.check);
			viewHolder.iconView = (ImageView) view.findViewById(R.id.icon);
			viewHolder.nameView = (TextView) view.findViewById(R.id.name);
			viewHolder.summaryView = (TextView) view.findViewById(R.id.summary);
			viewHolder.loadingView = (TextView) view.findViewById(R.id.loading);
			viewHolder.preventView = (ImageView) view.findViewById(R.id.prevent);
			viewHolder.checkView.setOnCheckedChangeListener(mContext);
			viewHolder.checkView.setTag(viewHolder);
			view.setTag(viewHolder);
		}

		ViewHolder holder = (ViewHolder) view.getTag();
		AppInfo appInfo = getItem(position);
		holder.packageName = appInfo.packageName;
		holder.nameView.setText(appInfo.name);
		holder.summaryView.setVisibility(View.GONE);
		holder.loadingView.setVisibility(View.VISIBLE);
		holder.checkView.setChecked(mContext.getSelection().contains(holder.packageName));
		Boolean result = mContext.getPackages().get(appInfo.packageName);
		if (result == null) {
			holder.preventView.setVisibility(View.INVISIBLE);
		} else {
			holder.preventView.setVisibility(View.VISIBLE);
			holder.preventView.setImageResource(result ? R.drawable.ic_menu_block : R.drawable.ic_menu_stop);
		}
		new AsyncTask<Object, Void, ViewHolder>() {
			@Override
			protected ViewHolder doInBackground(Object... params) {
				ViewHolder holder = (ViewHolder) params[0];
				AppInfo appInfo = (AppInfo) params[1];
				PackageManager pm = (PackageManager) params[2];
				holder.running = mContext.getRunningProcesses().get(appInfo.packageName);
				try {
					holder.icon = pm.getApplicationIcon(appInfo.packageName);
				} catch (NameNotFoundException e) {
				}
				return holder;
			}

			@Override
			protected void onPostExecute(ViewHolder holder) {
				holder.iconView.setImageDrawable(holder.icon);
				holder.loadingView.setVisibility(View.GONE);
				holder.summaryView.setVisibility(View.VISIBLE);
				holder.summaryView.setText(formatRunning(holder.running));
			}
		}.execute(holder, getItem(position), pm);
		return view;
	}

	private CharSequence formatRunning(Set<Integer> running) {
		if (running == null) {
			return mContext.getString(R.string.notrunning);
		} else {
			TreeSet<String> sets = new TreeSet<String>();
			for (Integer i : running) {
				switch (i) {
				case RunningAppProcessInfo.IMPORTANCE_BACKGROUND:
					sets.add(mContext.getString(R.string.background));
					break;
				case RunningAppProcessInfo.IMPORTANCE_EMPTY:
					sets.add(mContext.getString(R.string.empty));
					break;
				case RunningAppProcessInfo.IMPORTANCE_FOREGROUND:
					sets.add(mContext.getString(R.string.foreground));
					break;
				case RunningAppProcessInfo.IMPORTANCE_PERCEPTIBLE:
					sets.add(mContext.getString(R.string.perceptible));
					break;
				case RunningAppProcessInfo.IMPORTANCE_SERVICE:
					sets.add(mContext.getString(R.string.service));
					break;
				case RunningAppProcessInfo.IMPORTANCE_VISIBLE:
					sets.add(mContext.getString(R.string.visible));
					break;
				default:
					break;
				}
			}
			StringBuilder buffer = new StringBuilder();
			Iterator<?> it = sets.iterator();
			while (true) {
				buffer.append(it.next());
				if (it.hasNext()) {
					buffer.append(", ");
				} else {
					break;
				}
			}
			return buffer.toString();
		}
	};

}